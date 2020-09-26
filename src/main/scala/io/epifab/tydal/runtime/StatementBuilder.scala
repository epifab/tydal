package io.epifab.tydal.runtime

import java.sql

import cats.Monad
import cats.effect.{ContextShift, Resource, Sync}
import io.epifab.tydal._
import io.epifab.tydal.queries.CompiledQuery
import io.epifab.tydal.schema._
import io.epifab.tydal.utils.TaggedFind
import shapeless.ops.hlist.Tupler
import shapeless.{::, Generic, HList, HNil}

import scala.collection.{Factory, mutable}

sealed trait GenericStatement[InputRepr <: HList, Fields <: HList] { Self =>
  def query: String
  def toRunnable: InputRepr => RunnableStatement[Fields]

  def update(
    implicit
    statementExecutor: WriteStatementExecutor[sql.Connection, Fields]
  ): WriteStatement[InputRepr, Fields] =
    new WriteStatement(query, toRunnable)

  def select[OutputRepr <: HList, TaggedOutput <: HList](
    implicit
    taggedOutput: TagOutput[Fields, OutputRepr, TaggedOutput]
  ): ReadStatementStep0[InputRepr, Fields, OutputRepr, TaggedOutput] =
    new ReadStatementStep0(query, toRunnable)

  def contramap[Input2 <: HList](f: Input2 => InputRepr): GenericStatement[Input2, Fields] = new GenericStatement[Input2, Fields] {
    override def query: String = Self.query
    override def toRunnable: Input2 => RunnableStatement[Fields] = (input) => Self.toRunnable(f(input))
  }
}

object GenericStatement {
  def apply[InputRepr <: HList, Fields <: HList](q: String, f: InputRepr => RunnableStatement[Fields]): GenericStatement[InputRepr, Fields] =
    new GenericStatement[InputRepr, Fields] {
      override def query: String = q
      override def toRunnable: InputRepr => RunnableStatement[Fields] = f
    }
}

class WriteStatement[Input, Fields <: HList](
  val query: String,
  toRunnable: Input => RunnableStatement[Fields])(
  implicit statementExecutor: WriteStatementExecutor[sql.Connection, Fields]
) {

  def runP[P](input: P)(
    implicit
    literals: Literals[P, Input]
  ): Transaction[Int] = Transaction(toRunnable(literals(input)))

  def run[P, G <: Input](values: P)(implicit generic: Generic.Aux[P, G]): Transaction[Int] =
    Transaction(toRunnable(generic.to(values)))

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
  toOutput: OutputRepr => Output) {

  implicit private def executor[C[_]](implicit factory: Factory[Output, C[Output]], dataExtractor: DataExtractor[sql.ResultSet, Fields, OutputRepr]): StatementExecutor[sql.Connection, Fields, C[Output]] = new StatementExecutor[sql.Connection, Fields, C[Output]] {

    override def run[F[+_] : Sync : ContextShift](connection: sql.Connection, jdbcExecutor: JdbcExecutor, statement: RunnableStatement[Fields]): F[C[Output]] = {
      import cats.implicits._

      def resultSetResource(statement: sql.PreparedStatement): Resource[F, sql.ResultSet] =
        Resource.make(jdbcExecutor(statement.executeQuery()))(rs => jdbcExecutor(rs.close()))

      def recurse(resultSet: sql.ResultSet, builder: mutable.Builder[Output, C[Output]]): F[C[Output]] =
        jdbcExecutor(resultSet.next()).flatMap {
          case false =>
            Monad[F].pure(builder.result())

          case true =>
            jdbcExecutor(dataExtractor.extract(resultSet, statement.fields)).flatMap {
              case Right(element) => recurse(resultSet, builder.addOne(toOutput(element)))
              case Left(error) => Sync[F].raiseError(error)
            }
        }

      for {
        ps <- Jdbc.initStatement(connection, jdbcExecutor, statement.sql, statement.input)
        rs <- resultSetResource(ps).use(recurse(_, factory.newBuilder))
      } yield rs

    }
  }

  def as[C[_] <: Iterable[_]](
    implicit
    factory: Factory[Output, C[Output]],
    dataExtractor: DataExtractor[java.sql.ResultSet, Fields, OutputRepr]
  ): ReadStatement[Input, Output, C] = {
    val toTransaction = (runnableStatement: RunnableStatement[Fields]) => Transaction(runnableStatement)
    new ReadStatement(query, input => toTransaction(toRunnable(input)))
  }

  def asOption(
    implicit
    dataExtractor: DataExtractor[java.sql.ResultSet, Fields, OutputRepr]
  ): ReadStatement[Input, Output, Option] = {

    val toTransaction = (runnableStatement: RunnableStatement[Fields]) => Transaction(runnableStatement)(executor[Vector]).flatMap {
      case twoOrMoreResults if twoOrMoreResults.size > 1 => Transaction.failed(MultipleResultsError("Multiple results"))
      case vector => Transaction.successful(vector.headOption)
    }

    new ReadStatement(query, input => toTransaction(toRunnable(input)))
  }

  def single(
    implicit
    dataExtractor: DataExtractor[java.sql.ResultSet, Fields, OutputRepr]
  ): ReadStatement[Input, Output, cats.Id] = {

    val toTransaction = (runnableStatement: RunnableStatement[Fields]) => Transaction(runnableStatement)(executor[Vector]).flatMap {
      case emptyVector if emptyVector.isEmpty => Transaction.failed(NoResultsError("No results"))
      case twoOrMoreResults if twoOrMoreResults.size > 1 => Transaction.failed(MultipleResultsError("Multiple results"))
      case nonEmptyVector if nonEmptyVector.nonEmpty => Transaction.successful(nonEmptyVector.head)
    }

    new ReadStatement[Input, Output, cats.Id](query, input => toTransaction(toRunnable(input)))
  }

  def single(default: Output)(
    implicit
    dataExtractor: DataExtractor[java.sql.ResultSet, Fields, OutputRepr]
  ): ReadStatement[Input, Output, cats.Id] = {

    val toTransaction = (runnableStatement: RunnableStatement[Fields]) => Transaction(runnableStatement)(executor[Vector]).flatMap {
      case emptyVector if emptyVector.isEmpty => Transaction.successful(default)
      case twoOrMoreResults if twoOrMoreResults.size > 1 => Transaction.failed(MultipleResultsError("Multiple results"))
      case nonEmptyVector if nonEmptyVector.nonEmpty => Transaction.successful(nonEmptyVector.head)
    }

    new ReadStatement[Input, Output, cats.Id](query, input => toTransaction(toRunnable(input)))
  }
}


class ReadStatement[Input, Output, C[_]](val query: String, toTransaction: Input => Transaction[C[Output]]) {

  def runP[P](input: P)(
    implicit
    literals: Literals[P, Input]
  ): Transaction[C[Output]] = toTransaction(literals(input))

  def run[P, G <: Input](input: P)(implicit generic: Generic.Aux[P, G]): Transaction[C[Output]] =
    toTransaction(generic.to(input))

}


case class RunnableStatement[Fields <: HList](sql: String, input: List[Literal[_]], fields: Fields)

trait StatementBuilder[-Placeholders <: HList, InputRepr <: HList, OutputRepr <: HList] {
  def build(query: CompiledQuery[Placeholders, OutputRepr]): GenericStatement[InputRepr, OutputRepr]
}

object StatementBuilder {
  implicit def noPlaceholders[Output <: HList]: StatementBuilder[HNil, HNil, Output] =
    (query: CompiledQuery[HNil, Output]) =>
      GenericStatement(query.sql, _ => RunnableStatement(query.sql, List.empty, query.fields))

  implicit def namedPlaceholder[PType, PTag <: String with Singleton, Tail <: HList, TailInput <: HList, Output <: HList](
    implicit
    tail: StatementBuilder[Tail, TailInput, Output]
  ): StatementBuilder[(NamedPlaceholder[PType] As PTag) :: Tail, (PTag ~~> PType) :: TailInput, Output] =
    (query: CompiledQuery[(NamedPlaceholder[PType] As PTag) :: Tail, Output]) =>
      GenericStatement(query.sql, values => RunnableStatement(
        query.sql,
        tail
          .build(CompiledQuery(query.sql, query.placeholders.tail, query.fields))
          .toRunnable(values.tail)
          .input prepended values.head.toLiteral(query.placeholders.head.decoder, query.placeholders.head.encoder),
        query.fields
      ))

  implicit def literal[PType, Tail <: HList, TailInput <: HList, Output <: HList](
    implicit
    tail: StatementBuilder[Tail, TailInput, Output]
  ): StatementBuilder[Literal[PType] :: Tail, TailInput, Output] =
    (query: CompiledQuery[Literal[PType] :: Tail, Output]) =>
      GenericStatement(query.sql, values => {
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
      GenericStatement(query.sql, values => {
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
