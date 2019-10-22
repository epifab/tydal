package io.epifab.yadl.typesafe

import java.sql.Connection

import io.epifab.yadl.typesafe.fields.{FieldT, Placeholder, Value}
import io.epifab.yadl.typesafe.runner.StatementExecutor
import shapeless.{::, HList, HNil}

class CompiledStatement[INPUT <: HList, OUTPUT <: HList](val withValues: INPUT => Statement[OUTPUT])

case class Statement[FIELDS <: HList](sql: String, input: Seq[Value[_]], fields: FIELDS) {
  def runSync[OUTPUT](connection: Connection)(implicit statementExecutor: StatementExecutor[Either, Connection, FIELDS, OUTPUT]): Either[DataError, Seq[OUTPUT]] =
    statementExecutor.run(connection, this)
}

trait StatementBuilder[PLACEHOLDERS <: HList, INPUT <: HList, OUTPUT <: HList] {
  def build(query: Query[PLACEHOLDERS, OUTPUT]): CompiledStatement[INPUT, OUTPUT]
}

object StatementBuilder {
  implicit def noPlaceholders[OUTPUT <: HList]: StatementBuilder[HNil, HNil, OUTPUT] =
    (query: Query[HNil, OUTPUT]) =>
      new CompiledStatement(_ => Statement(query.sql, Seq.empty, query.fields))

  implicit def placeholders[P <: Placeholder[_, _] with Tag[_], PTYPE, PTAG <: String, TAIL <: HList, TAIL_INPUT <: HList, OUTPUT <: HList]
      (implicit
       tagged: Tagged[P, PTAG],
       fieldT: FieldT[P, PTYPE],
       tail: StatementBuilder[TAIL, TAIL_INPUT, OUTPUT]): StatementBuilder[P :: TAIL, Value[PTYPE] with Tag[PTAG] :: TAIL_INPUT, OUTPUT] =
    (query: Query[P :: TAIL, OUTPUT]) =>
      new CompiledStatement(values => Statement(
          query.sql,
          tail
            .build(Query(query.sql, query.placeholders.tail, query.fields))
            .withValues(values.tail)
            .input prepended values.head,
          query.fields
        )
      )
}
