package io.epifab.yadl.typesafe.runner

import java.sql.Connection

import io.epifab.yadl.typesafe.fields.{FieldT, Placeholder, Value}
import io.epifab.yadl.typesafe.{DataError, Query, Tag, Tagged}
import shapeless.ops.hlist.Tupler
import shapeless.{::, Generic, HList, HNil, the}

class CompiledStatement[RAW_INPUT <: HList, INPUT, OUTPUT <: HList](val rawWithValues: RAW_INPUT => Statement[OUTPUT])(implicit tupler: Tupler.Aux[RAW_INPUT, INPUT]) {
  def withValues(values: INPUT)(implicit generic: Generic.Aux[INPUT, RAW_INPUT]): Statement[OUTPUT] =
    rawWithValues(generic.to(values))
}

case class Statement[FIELDS <: HList](sql: String, input: Seq[Value[_]], fields: FIELDS) {
  def runSync[OUTPUT](connection: Connection)(implicit statementExecutor: StatementExecutor[Either, Connection, FIELDS, OUTPUT]): Either[DataError, Seq[OUTPUT]] =
    statementExecutor.run(connection, this)

  def run[F[+_, +_], CONN](connection: CONN)(implicit statementExecutor: StatementExecutor[F, CONN, FIELDS, FIELDS]): F[DataError, Seq[FIELDS]] =
    statementExecutor.run(connection, this)
}

trait StatementBuilder[PLACEHOLDERS <: HList, RAW_INPUT <: HList, INPUT, OUTPUT <: HList] {
  def build(query: Query[PLACEHOLDERS, OUTPUT]): CompiledStatement[RAW_INPUT, INPUT, OUTPUT]
}

object StatementBuilder {
  implicit def noPlaceholders[OUTPUT <: HList]: StatementBuilder[HNil, HNil, Unit, OUTPUT] =
    (query: Query[HNil, OUTPUT]) =>
      new CompiledStatement(_ => Statement(query.sql, Seq.empty, query.fields))

  implicit def placeholders[P <: Placeholder[_, _] with Tag[_], PTYPE, PTAG <: String, TAIL <: HList, TAIL_INPUT <: HList, OUTPUT <: HList, INPUT_TUPLE]
      (implicit
       tagged: Tagged[P, PTAG],
       fieldT: FieldT[P, PTYPE],
       tail: StatementBuilder[TAIL, TAIL_INPUT, _, OUTPUT],
       tupler: Tupler.Aux[Value[PTYPE] with Tag[PTAG] :: TAIL_INPUT, INPUT_TUPLE]
      ): StatementBuilder[P :: TAIL, Value[PTYPE] with Tag[PTAG] :: TAIL_INPUT, INPUT_TUPLE, OUTPUT] =
    (query: Query[P :: TAIL, OUTPUT]) =>
      new CompiledStatement(values => Statement(
          query.sql,
          tail
            .build(Query(query.sql, query.placeholders.tail, query.fields))
            .rawWithValues(values.tail)
            .input prepended values.head,
          query.fields
        )
      )
}
