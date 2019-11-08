package io.epifab.tydal.runner

import java.sql.Connection

import cats.effect.IO
import io.epifab.tydal._
import io.epifab.tydal.fields.{FieldT, NamedPlaceholder, PlaceholderValueOption, PlaceholderValue}
import shapeless.ops.hlist.Tupler
import shapeless.{::, Generic, HList, HNil}

import scala.collection.{Factory, mutable}

class GenericStatement[RAW_INPUT <: HList, INPUT, FIELDS <: HList]
(val query: String, val toRunnable: RAW_INPUT => RunnableStatement[FIELDS])
(implicit tupler: Tupler.Aux[RAW_INPUT, INPUT], generic: Generic.Aux[INPUT, RAW_INPUT]) {
  def update: WriteStatement[INPUT, FIELDS] =
    new WriteStatement(query, (generic.to _).andThen(toRunnable))
  def select: ReadStatementStep1[INPUT, FIELDS] =
    new ReadStatementStep1(query, (generic.to _).andThen(toRunnable))
}

class WriteStatement[INPUT, FIELDS <: HList]
(val query: String, toRunnable: INPUT => RunnableStatement[FIELDS]) {
  def withValues
  (values: INPUT)
  (implicit statementExecutor: WriteStatementExecutor[IOEither, Connection, FIELDS]):
  TransactionIO[Int] = TransactionIO(toRunnable(values))
}

class ReadStatementStep1[INPUT, FIELDS <: HList]
(val query: String, toRunnable: INPUT => RunnableStatement[FIELDS]) {
  def withValues[RAW_OUTPUT <: HList](values: INPUT)(implicit statementExecutor: ReadStatementExecutor[IOEither, Connection, FIELDS, RAW_OUTPUT]): ReadStatementStep2[FIELDS, RAW_OUTPUT] =
    new ReadStatementStep2[FIELDS, RAW_OUTPUT](toRunnable(values))
}

class ReadStatementStep2[FIELDS <: HList, RAW_OUTPUT <: HList]
(runnableStatement: RunnableStatement[FIELDS])
(implicit readStatementExecutor: ReadStatementExecutor[IOEither, Connection, FIELDS, RAW_OUTPUT]) {
  def mapTo[OUTPUT](implicit generic: Generic.Aux[OUTPUT, RAW_OUTPUT]): ReadStatementStep3[FIELDS, RAW_OUTPUT, OUTPUT] =
    new ReadStatementStep3(runnableStatement, generic.from)

  def tuple[TUPLE](implicit tupler: Tupler.Aux[RAW_OUTPUT, TUPLE], generic: Generic.Aux[TUPLE, RAW_OUTPUT]): ReadStatementStep3[FIELDS, RAW_OUTPUT, TUPLE] =
    new ReadStatementStep3(runnableStatement, generic.from)
}


class ReadStatementStep3[FIELDS <: HList, RAW_OUTPUT, OUTPUT]
(runnableStatement: RunnableStatement[FIELDS], toOutput: RAW_OUTPUT => OUTPUT)
(implicit readStatementExecutor: ReadStatementExecutor[IOEither, Connection, FIELDS, RAW_OUTPUT]) {
  def as[C[_]](implicit factory: Factory[OUTPUT, C[OUTPUT]]): TransactionIO[C[OUTPUT]] =
    TransactionIO(runnableStatement) {
      new StatementExecutor[IOEither, Connection, FIELDS, C[OUTPUT]] {
        override def run(connection: Connection, statement: RunnableStatement[FIELDS]): IO[Either[DataError, C[OUTPUT]]] =
          readStatementExecutor.run(connection, statement).map {
            case Left(dataError) => Left(dataError)
            case Right(iterator) =>
              val errorOrBuilder = iterator.foldLeft[Either[DataError, mutable.Builder[OUTPUT, C[OUTPUT]]]](Right(factory.newBuilder)) {
                case (Left(e), _) => Left(e)
                case (_, Left(e)) => Left(e)
                case (Right(builder), Right(x)) => Right(builder.addOne(toOutput(x)))
              }
              errorOrBuilder.map(_.result)
          }
      }
    }

  def option: TransactionIO[Option[OUTPUT]] =
    TransactionIO(runnableStatement) {
      new StatementExecutor[IOEither, Connection, FIELDS, Option[OUTPUT]] {
        override def run(connection: Connection, statement: RunnableStatement[FIELDS]): IO[Either[DataError, Option[OUTPUT]]] =
          readStatementExecutor.run(connection, statement).map {
            case Left(dataError) => Left(dataError)
            case Right(iterator) =>
              iterator.foldLeft[Either[DataError, Option[OUTPUT]]](Right(None)) {
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

case class RunnableStatement[FIELDS <: HList](sql: String, input: Seq[PlaceholderValue[_]], fields: FIELDS)

trait StatementBuilder[PLACEHOLDERS <: HList, RAW_INPUT <: HList, INPUT, OUTPUT <: HList] {
  def build(query: Query[PLACEHOLDERS, OUTPUT]): GenericStatement[RAW_INPUT, INPUT, OUTPUT]
}

object StatementBuilder {
  implicit def noPlaceholders[OUTPUT <: HList]: StatementBuilder[HNil, HNil, Unit, OUTPUT] =
    (query: Query[HNil, OUTPUT]) =>
      new GenericStatement(query.sql, _ => RunnableStatement(query.sql, Seq.empty, query.fields))

  implicit def namedPlaceholder[P <: NamedPlaceholder[_] with Tag[_], PTYPE, PTAG <: String, TAIL <: HList, TAIL_INPUT <: HList, OUTPUT <: HList, INPUT_TUPLE]
      (implicit
       tagged: Tagged[P, PTAG],
       fieldT: FieldT[P, PTYPE],
       tail: StatementBuilder[TAIL, TAIL_INPUT, _, OUTPUT],
       tupler: Tupler.Aux[PlaceholderValue[PTYPE] with Tag[PTAG] :: TAIL_INPUT, INPUT_TUPLE],
       generic: Generic.Aux[INPUT_TUPLE, PlaceholderValue[PTYPE] with Tag[PTAG] :: TAIL_INPUT]
      ): StatementBuilder[P :: TAIL, PlaceholderValue[PTYPE] with Tag[PTAG] :: TAIL_INPUT, INPUT_TUPLE, OUTPUT] =
    (query: Query[P :: TAIL, OUTPUT]) =>
      new GenericStatement(query.sql, values => RunnableStatement(
          query.sql,
          tail
            .build(Query(query.sql, query.placeholders.tail, query.fields))
            .toRunnable(values.tail)
            .input prepended values.head,
          query.fields
        )
      )

  implicit def value[P <: PlaceholderValue[_], PTYPE, TAIL <: HList, TAIL_INPUT <: HList, OUTPUT <: HList, INPUT_TUPLE]
      (implicit
       fieldT: FieldT[P, PTYPE],
       tail: StatementBuilder[TAIL, TAIL_INPUT, _, OUTPUT],
       tupler: Tupler.Aux[TAIL_INPUT, INPUT_TUPLE],
       generic: Generic.Aux[INPUT_TUPLE, TAIL_INPUT]
      ): StatementBuilder[P :: TAIL, TAIL_INPUT, INPUT_TUPLE, OUTPUT] =
    (query: Query[P :: TAIL, OUTPUT]) =>
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

  implicit def optionalValue[P <: PlaceholderValueOption[_], PTYPE, TAIL <: HList, TAIL_INPUT <: HList, OUTPUT <: HList, INPUT_TUPLE]
      (implicit
       fieldT: FieldT[P, PTYPE],
       tail: StatementBuilder[TAIL, TAIL_INPUT, _, OUTPUT],
       tupler: Tupler.Aux[TAIL_INPUT, INPUT_TUPLE],
       generic: Generic.Aux[INPUT_TUPLE, TAIL_INPUT]
      ): StatementBuilder[P :: TAIL, TAIL_INPUT, INPUT_TUPLE, OUTPUT] =
    (query: Query[P :: TAIL, OUTPUT]) =>
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
