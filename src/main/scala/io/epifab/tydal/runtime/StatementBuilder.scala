package io.epifab.tydal.runtime

import java.sql.Connection

import cats.Monad
import cats.implicits._
import io.epifab.tydal._
import io.epifab.tydal.queries.CompiledQuery
import io.epifab.tydal.schema._
import io.epifab.tydal.utils.{Finder, TaggedFinder}
import shapeless.ops.hlist.Tupler
import shapeless.{::, Generic, HList, HNil}

import scala.collection.{Factory, mutable}

class GenericStatement[InputRepr <: HList, Input, Fields <: HList](
  val query: String,
  val toRunnable: InputRepr => RunnableStatement[Fields])(
  implicit
  tupler: Tupler.Aux[InputRepr, Input],
  generic: Generic.Aux[Input, InputRepr]
) {

  def update: WriteStatement[Input, Fields] =
    new WriteStatement(query, (generic.to _).andThen(toRunnable))

  def select[OutputRepr <: HList, TaggedOutput <: HList](
    implicit
    readStatement: ReadStatementExecutor[Connection, Fields, OutputRepr],
    taggedOutput: TagOutput[Fields, OutputRepr, TaggedOutput]
  ): ReadStatementStep0[Input, Fields, OutputRepr, TaggedOutput] =
    new ReadStatementStep0(query, (generic.to _).andThen(toRunnable))
}

class WriteStatement[InputTuple, Fields <: HList](
  val query: String,
  toRunnable: InputTuple => RunnableStatement[Fields]
) {

  def run(
    values: InputTuple)(
    implicit
    statementExecutor: WriteStatementExecutor[Connection, Fields]
  ): Transaction[Int] = Transaction(toRunnable(values))

  def run[P, InputRepr <: HList](
    values: P)(
    implicit
    statementExecutor: WriteStatementExecutor[Connection, Fields],
    placeholderValues: PlaceholderValues[P, InputRepr],
    tupler: Generic.Aux[InputTuple, InputRepr]
  ): Transaction[Int] = run(tupler.from(placeholderValues(values)))

}

trait TagOutput[Fields, OutputRepr, Tagged] {
  def apply(outputRepr: OutputRepr): Tagged
}

object TagOutput {
  implicit def head[F <: Field[_] with Tagging[_], A <: String with Singleton, T, FTail <: HList, OTail <: HList, TaggedTail <: HList](
    implicit
    tagged: Tagged[F, A],
    fieldT: FieldT[F, T],
    tail: TagOutput[FTail, OTail, TaggedTail]
  ): TagOutput[F :: FTail, T :: OTail, (T As A) :: TaggedTail] =
    (o: T :: OTail) => o.head.asInstanceOf[T As A] :: tail(o.tail)

  implicit val hNil: TagOutput[HNil, HNil, HNil] =
    (_: HNil) => HNil
}

class ResultSet[TaggedOutput](output: TaggedOutput) {
  def apply[A <: String with Singleton, T](a: A)(implicit finder: TaggedFinder[A, T, TaggedOutput]): T =
    finder.find(output)
}

class ReadStatementStep0[Input, Fields <: HList, OutputRepr <: HList, TaggedOutput](
  val query: String,
  toRunnable: Input => RunnableStatement[Fields])(
  implicit
  readStatement: ReadStatementExecutor[Connection, Fields, OutputRepr],
  taggedOutput: TagOutput[Fields, OutputRepr, TaggedOutput]
) {

  def to[Output](implicit generic: Generic.Aux[Output, OutputRepr]): ReadStatementStep1[Input, Fields, OutputRepr, Output] =
    new ReadStatementStep1(query, toRunnable, generic.from)

  def toTuple[Tuple](implicit tupler: Tupler.Aux[OutputRepr, Tuple], generic: Generic.Aux[Tuple, OutputRepr]): ReadStatementStep1[Input, Fields, OutputRepr, Tuple] =
    new ReadStatementStep1(query, toRunnable, generic.from)

  def rawTo[Output](map: OutputRepr => Output): ReadStatementStep1[Input, Fields, OutputRepr, Output] =
    new ReadStatementStep1(query, toRunnable, map)

  def to[Output](map: ResultSet[TaggedOutput] => Output): ReadStatementStep1[Input, Fields, OutputRepr, Output] =
    new ReadStatementStep1(query, toRunnable, outputRepr => map(new ResultSet(taggedOutput(outputRepr))))

}

class ReadStatementStep1[Input, Fields <: HList, OutputRepr <: HList, Output](
  val query: String,
  toRunnable: Input => RunnableStatement[Fields],
  toOutput: OutputRepr => Output)(
  implicit
  readStatement: ReadStatementExecutor[Connection, Fields, OutputRepr]
) {

  def as[C[_]](implicit factory: Factory[Output, C[Output]]): ReadStatement[Input, Output, C] = {
    val toTransaction = (runnableStatement: RunnableStatement[Fields]) => Transaction(runnableStatement) {
      new StatementExecutor[Connection, Fields, C[Output]] {
        override def run[F[+_]: Eff: Monad](connection: Connection, statement: RunnableStatement[Fields]): F[Either[DataError, C[Output]]] =
          readStatement.run(connection, statement).map {
            case Left(dataError) => Left(dataError)
            case Right(iterator) =>
              val errorOrBuilder = iterator.foldLeft[Either[DataError, mutable.Builder[Output, C[Output]]]](Right(factory.newBuilder)) {
                case (Left(e), _) => Left(e)
                case (_, Left(e)) => Left(e)
                case (Right(builder), Right(x)) => Right(builder.addOne(toOutput(x)))
              }
              errorOrBuilder.map(_.result)
          }
      }
    }

    new ReadStatement(query, input => toTransaction(toRunnable(input)))
  }

  def asOption: ReadStatement[Input, Output, Option] = {
    val toTransaction = (runnableStatement: RunnableStatement[Fields]) => Transaction(runnableStatement) {
      new StatementExecutor[Connection, Fields, Option[Output]] {
        override def run[F[+_]: Eff: Monad](connection: Connection, statement: RunnableStatement[Fields]): F[Either[DataError, Option[Output]]] =
          readStatement.run(connection, statement).map {
            case Left(dataError) => Left(dataError)
            case Right(iterator) =>
              iterator.foldLeft[Either[DataError, Option[Output]]](Right(None)) {
                case (Left(e), _) => Left(e)
                case (_, Left(e)) => Left(e)
                case (Right(None), Right(x)) => Right(Some(toOutput(x)))
                case (Right(Some(old)), Right(x)) =>
                  Left(MultipleResultsError(s"Only one result was expected, multiple returned: $old - $x"))
              }
          }
      }
    }

    new ReadStatement(query, input => toTransaction(toRunnable(input)))
  }
}


class ReadStatement[Input, Output, C[_]](val query: String, toTransaction: Input => Transaction[C[Output]]) {
  def run(input: Input): Transaction[C[Output]] = toTransaction(input)
}


case class RunnableStatement[Fields <: HList](sql: String, input: Seq[Literal[_]], fields: Fields)

trait StatementBuilder[Placeholders <: HList, InputRepr <: HList, Input, Output <: HList] {
  def build(query: CompiledQuery[Placeholders, Output]): GenericStatement[InputRepr, Input, Output]
}

object StatementBuilder {
  implicit def noPlaceholders[Output <: HList]: StatementBuilder[HNil, HNil, Unit, Output] =
    (query: CompiledQuery[HNil, Output]) =>
      new GenericStatement(query.sql, _ => RunnableStatement(query.sql, Seq.empty, query.fields))

  implicit def namedPlaceholder[P <: NamedPlaceholder[_] with Tagging[_], PTYPE, PTAG <: String with Singleton, Tail <: HList, TailInput <: HList, Output <: HList, InputTuple]
      (implicit
       tagged: Tagged[P, PTAG],
       fieldT: FieldT[P, PTYPE],
       tail: StatementBuilder[Tail, TailInput, _, Output],
       tupler: Tupler.Aux[(PTAG ~~> PTYPE) :: TailInput, InputTuple],
       generic: Generic.Aux[InputTuple, (PTAG ~~> PTYPE) :: TailInput]
      ): StatementBuilder[P :: Tail, (PTAG ~~> PTYPE) :: TailInput, InputTuple, Output] =
    (query: CompiledQuery[P :: Tail, Output]) =>
      new GenericStatement(query.sql, values => RunnableStatement(
          query.sql,
          tail
            .build(CompiledQuery(query.sql, query.placeholders.tail, query.fields))
            .toRunnable(values.tail)
            .input prepended values.head,
          query.fields
        )
      )

  implicit def value[P <: Literal[_], PType, Tail <: HList, TailInput <: HList, Output <: HList, InputTuple]
      (implicit
       fieldT: FieldT[P, PType],
       tail: StatementBuilder[Tail, TailInput, _, Output],
       tupler: Tupler.Aux[TailInput, InputTuple],
       generic: Generic.Aux[InputTuple, TailInput]
      ): StatementBuilder[P :: Tail, TailInput, InputTuple, Output] =
    (query: CompiledQuery[P :: Tail, Output]) =>
      new GenericStatement(query.sql, values => {
        RunnableStatement(
          query.sql,
          tail
            .build(CompiledQuery(query.sql, query.placeholders.tail, query.fields))
            .toRunnable(values)
            .input prepended query.placeholders.head,
          query.fields
        )
      })

  implicit def optionalValue[P <: LiteralOption[_], PTYPE, Tail <: HList, TailInput <: HList, Output <: HList, InputTuple]
      (implicit
       fieldT: FieldT[P, PTYPE],
       tail: StatementBuilder[Tail, TailInput, _, Output],
       tupler: Tupler.Aux[TailInput, InputTuple],
       generic: Generic.Aux[InputTuple, TailInput]
      ): StatementBuilder[P :: Tail, TailInput, InputTuple, Output] =
    (query: CompiledQuery[P :: Tail, Output]) =>
      new GenericStatement(query.sql, values => {
        val tailValues = tail
          .build(CompiledQuery(query.sql, query.placeholders.tail, query.fields))
          .toRunnable(values)
          .input

        val valueSeq = query.placeholders.head.value
          .map(tailValues.prepended)
          .getOrElse(tailValues)

        RunnableStatement(
          query.sql,
          valueSeq,
          query.fields
        )
      })
}
