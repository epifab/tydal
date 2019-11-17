package io.epifab.tydal.runner

import java.sql.Connection

import cats.implicits._
import cats.Monad
import cats.effect.{IO, Sync}
import io.epifab.tydal._
import io.epifab.tydal.fields.{FieldT, NamedPlaceholder, PlaceholderValue, PlaceholderValueOption}
import shapeless.ops.hlist.Tupler
import shapeless.{::, Generic, HList, HNil}

import scala.collection.{Factory, mutable}

class GenericStatement[InputRepr <: HList, Input, Fields <: HList]
(val query: String, val toRunnable: InputRepr => RunnableStatement[Fields])
(implicit tupler: Tupler.Aux[InputRepr, Input], generic: Generic.Aux[Input, InputRepr]) {
  def update: WriteStatement[Input, Fields] =
    new WriteStatement(query, (generic.to _).andThen(toRunnable))
  def select: ReadStatementStep1[Input, Fields] =
    new ReadStatementStep1(query, (generic.to _).andThen(toRunnable))
}

class WriteStatement[Input, Fields <: HList]
(val query: String, toRunnable: Input => RunnableStatement[Fields]) {
  def withValues
  (values: Input)
  (implicit statementExecutor: WriteStatementExecutor[Connection, Fields]):
  Transaction[Int] = Transaction(toRunnable(values))
}

class ReadStatementStep1[Input, Fields <: HList]
(val query: String, toRunnable: Input => RunnableStatement[Fields]) {
  def withValues[OutputRepr <: HList](values: Input)(implicit statementExecutor: ReadStatementExecutor[Connection, Fields, OutputRepr]): ReadStatementStep2[Fields, OutputRepr] =
    new ReadStatementStep2[Fields, OutputRepr](toRunnable(values))
}

class ReadStatementStep2[Fields <: HList, OutputRepr <: HList]
(runnableStatement: RunnableStatement[Fields])
(implicit readStatementExecutor: ReadStatementExecutor[Connection, Fields, OutputRepr]) {
  def mapTo[Output](implicit generic: Generic.Aux[Output, OutputRepr]): ReadStatementStep3[Fields, OutputRepr, Output] =
    new ReadStatementStep3(runnableStatement, generic.from)

  def tuple[TUPLE](implicit tupler: Tupler.Aux[OutputRepr, TUPLE], generic: Generic.Aux[TUPLE, OutputRepr]): ReadStatementStep3[Fields, OutputRepr, TUPLE] =
    new ReadStatementStep3(runnableStatement, generic.from)
}


class ReadStatementStep3[Fields <: HList, OutputRepr, Output]
(runnableStatement: RunnableStatement[Fields], toOutput: OutputRepr => Output)
(implicit readStatementExecutor: ReadStatementExecutor[Connection, Fields, OutputRepr]) {
  def as[C[_]](implicit factory: Factory[Output, C[Output]]): Transaction[C[Output]] =
    Transaction(runnableStatement) {
      new StatementExecutor[Connection, Fields, C[Output]] {
        override def run[F[+_]: Eff: Monad](connection: Connection, statement: RunnableStatement[Fields]): F[Either[DataError, C[Output]]] =
          readStatementExecutor.run(connection, statement).map {
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

  def option: Transaction[Option[Output]] =
    Transaction(runnableStatement) {
      new StatementExecutor[Connection, Fields, Option[Output]] {
        override def run[F[+_]: Eff: Monad](connection: Connection, statement: RunnableStatement[Fields]): F[Either[DataError, Option[Output]]] =
          readStatementExecutor.run(connection, statement).map {
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
}

case class RunnableStatement[Fields <: HList](sql: String, input: Seq[PlaceholderValue[_]], fields: Fields)

trait StatementBuilder[Placeholders <: HList, InputRepr <: HList, Input, Output <: HList] {
  def build(query: Query[Placeholders, Output]): GenericStatement[InputRepr, Input, Output]
}

object StatementBuilder {
  implicit def noPlaceholders[Output <: HList]: StatementBuilder[HNil, HNil, Unit, Output] =
    (query: Query[HNil, Output]) =>
      new GenericStatement(query.sql, _ => RunnableStatement(query.sql, Seq.empty, query.fields))

  implicit def namedPlaceholder[P <: NamedPlaceholder[_] with Tagging[_], PTYPE, PTAG <: String with Singleton, Tail <: HList, TailInput <: HList, Output <: HList, InputTuple]
      (implicit
       tagged: Tagged[P, PTAG],
       fieldT: FieldT[P, PTYPE],
       tail: StatementBuilder[Tail, TailInput, _, Output],
       tupler: Tupler.Aux[PlaceholderValue[PTYPE] with Tagging[PTAG] :: TailInput, InputTuple],
       generic: Generic.Aux[InputTuple, PlaceholderValue[PTYPE] with Tagging[PTAG] :: TailInput]
      ): StatementBuilder[P :: Tail, PlaceholderValue[PTYPE] with Tagging[PTAG] :: TailInput, InputTuple, Output] =
    (query: Query[P :: Tail, Output]) =>
      new GenericStatement(query.sql, values => RunnableStatement(
          query.sql,
          tail
            .build(Query(query.sql, query.placeholders.tail, query.fields))
            .toRunnable(values.tail)
            .input prepended values.head,
          query.fields
        )
      )

  implicit def value[P <: PlaceholderValue[_], PType, Tail <: HList, TailInput <: HList, Output <: HList, InputTuple]
      (implicit
       fieldT: FieldT[P, PType],
       tail: StatementBuilder[Tail, TailInput, _, Output],
       tupler: Tupler.Aux[TailInput, InputTuple],
       generic: Generic.Aux[InputTuple, TailInput]
      ): StatementBuilder[P :: Tail, TailInput, InputTuple, Output] =
    (query: Query[P :: Tail, Output]) =>
      new GenericStatement(query.sql, values => {
        RunnableStatement(
          query.sql,
          tail
            .build(Query(query.sql, query.placeholders.tail, query.fields))
            .toRunnable(values)
            .input prepended query.placeholders.head,
          query.fields
        )
      })

  implicit def optionalValue[P <: PlaceholderValueOption[_], PTYPE, Tail <: HList, TailInput <: HList, Output <: HList, InputTuple]
      (implicit
       fieldT: FieldT[P, PTYPE],
       tail: StatementBuilder[Tail, TailInput, _, Output],
       tupler: Tupler.Aux[TailInput, InputTuple],
       generic: Generic.Aux[InputTuple, TailInput]
      ): StatementBuilder[P :: Tail, TailInput, InputTuple, Output] =
    (query: Query[P :: Tail, Output]) =>
      new GenericStatement(query.sql, values => {
        val tailValues = tail
          .build(Query(query.sql, query.placeholders.tail, query.fields))
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
