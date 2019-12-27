package io.epifab.tydal.runtime

import java.sql.Connection

import cats.Monad
import cats.effect.Sync
import cats.implicits._
import io.epifab.tydal._
import io.epifab.tydal.queries.CompiledQuery
import io.epifab.tydal.schema._
import io.epifab.tydal.utils.{Finder, TaggedFind}
import shapeless.ops.hlist.Tupler
import shapeless.{::, Generic, HList, HNil}

import scala.collection.{Factory, mutable}

class GenericStatement[InputRepr <: HList, Fields <: HList](
  val query: String,
  val toRunnable: InputRepr => RunnableStatement[Fields]) {

  def update: WriteStatement[InputRepr, Fields] =
    new WriteStatement(query, toRunnable)

  def select[OutputRepr <: HList, TaggedOutput <: HList](
    implicit
    readStatement: ReadStatementExecutor[Connection, Fields, OutputRepr],
    taggedOutput: TagOutput[Fields, OutputRepr, TaggedOutput]
  ): ReadStatementStep0[InputRepr, Fields, OutputRepr, TaggedOutput] =
    new ReadStatementStep0(query, toRunnable)
}

class WriteStatement[InputRepr <: HList, Fields <: HList](
  val query: String,
  toRunnable: InputRepr => RunnableStatement[Fields]
) {

  def runP[P](values: P)(
    implicit
    statementExecutor: WriteStatementExecutor[Connection, Fields],
    placeholderValues: PlaceholderValues[P, InputRepr]
  ): Transaction[Int] = Transaction(toRunnable(placeholderValues(values)))

  def run[P](values: P)(
    implicit
    generic: Generic.Aux[P, InputRepr],
    statementExecutor: WriteStatementExecutor[Connection, Fields],
  ): Transaction[Int] = Transaction(toRunnable(generic.to(values)))

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
  def apply[A <: String with Singleton, T](a: A)(implicit find: TaggedFind[A, T, TaggedOutput]): T =
    find(output)
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

  def head[Head, Tail <: HList](implicit head: OutputRepr =:= (Head :: Tail)): ReadStatementStep1[Input, Fields, OutputRepr, Head] =
    new ReadStatementStep1(query, toRunnable, _.head)

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
        override def run[F[+_]: Sync : Monad](connection: Connection, statement: RunnableStatement[Fields]): F[Either[DataError, C[Output]]] =
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
        override def run[F[+_]: Sync : Monad](connection: Connection, statement: RunnableStatement[Fields]): F[Either[DataError, Option[Output]]] =
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

  def last(default: Output): ReadStatement[Input, Output, cats.Id] = {
    val toTransaction = (runnableStatement: RunnableStatement[Fields]) => Transaction(runnableStatement) {
      new StatementExecutor[Connection, Fields, Output] {
        override def run[F[+_]: Sync : Monad](connection: Connection, statement: RunnableStatement[Fields]): F[Either[DataError, Output]] =
          readStatement.run(connection, statement).map {
            case Left(dataError) => Left(dataError)
            case Right(iterator) =>
              iterator.foldLeft[Either[DataError, Output]](Right(default)) {
                case (Left(e), _) => Left(e)
                case (_, Left(e)) => Left(e)
                case (Right(_), Right(x)) => Right(toOutput(x))
              }
          }
      }
    }

    new ReadStatement[Input, Output, cats.Id](query, input => toTransaction(toRunnable(input)))
  }
}


class ReadStatement[InputRepr, Output, C[_]](val query: String, toTransaction: InputRepr => Transaction[C[Output]]) {
  def run[P](input: P)(implicit generic: Generic.Aux[P, InputRepr]): Transaction[C[Output]] =
    toTransaction(generic.to(input))
}


case class RunnableStatement[Fields <: HList](sql: String, input: Seq[Literal[_]], fields: Fields)

trait StatementBuilder[-Placeholders <: HList, InputRepr <: HList, OutputRepr <: HList] {
  def build(query: CompiledQuery[Placeholders, OutputRepr]): GenericStatement[InputRepr, OutputRepr]
}

object StatementBuilder {
  implicit def noPlaceholders[Output <: HList]: StatementBuilder[HNil, HNil, Output] =
    (query: CompiledQuery[HNil, Output]) =>
      new GenericStatement(query.sql, _ => RunnableStatement(query.sql, Seq.empty, query.fields))

  implicit def namedPlaceholder[PType, PTag <: String with Singleton, Tail <: HList, TailInput <: HList, Output <: HList](
    implicit
    tail: StatementBuilder[Tail, TailInput, Output]
  ): StatementBuilder[(NamedPlaceholder[PType] As PTag) :: Tail, (PTag ~~> PType) :: TailInput, Output] =
    (query: CompiledQuery[(NamedPlaceholder[PType] As PTag) :: Tail, Output]) =>
      new GenericStatement(query.sql, values => RunnableStatement(
          query.sql,
          tail
            .build(CompiledQuery(query.sql, query.placeholders.tail, query.fields))
            .toRunnable(values.tail)
            .input prepended values.head,
          query.fields
        )
      )

  implicit def literal[PType, Tail <: HList, TailInput <: HList, Output <: HList](
    implicit
    tail: StatementBuilder[Tail, TailInput, Output]
  ): StatementBuilder[Literal[PType] :: Tail, TailInput, Output] =
    (query: CompiledQuery[Literal[PType] :: Tail, Output]) =>
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

  implicit def literalOption[PType, Tail <: HList, TailInput <: HList, Output <: HList](
    implicit
    tail: StatementBuilder[Tail, TailInput, Output],
  ): StatementBuilder[LiteralOption[PType] :: Tail, TailInput, Output] =
    (query: CompiledQuery[LiteralOption[PType] :: Tail, Output]) =>
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
