package io.epifab.yadl.typesafe.runner

import io.epifab.yadl.typesafe._
import io.epifab.yadl.typesafe.fields.{FieldT, Placeholder, Value}
import shapeless.ops.hlist.Tupler
import shapeless.{::, Generic, HList, HNil}

class Statement[RAW_INPUT <: HList, INPUT, FIELDS <: HList](val toRunnable: RAW_INPUT => RunnableStatement[FIELDS])(implicit tupler: Tupler.Aux[RAW_INPUT, INPUT], generic: Generic.Aux[INPUT, RAW_INPUT]) {
  def run[CONN, RAW_OUTPUT <: HList](connection: CONN)(values: INPUT)(implicit statementExecutor: StatementExecutor[IOEither, CONN, FIELDS, RAW_OUTPUT]): SeqOfResult[RAW_OUTPUT] =
    new SeqOfResult(statementExecutor.run(connection, toRunnable(generic.to(values))))
}

class SeqOfResult[RAW_OUTPUT <: HList](results: IOEither[DataError, Seq[RAW_OUTPUT]]) {
  def takeFirst: OptionalResult[RAW_OUTPUT] = new OptionalResult(results.map(_.map(_.headOption)))

  def mapTo[OUTPUT](implicit generic: Generic.Aux[OUTPUT, RAW_OUTPUT]): IOEither[DataError, Seq[OUTPUT]] =
    results.map(_.map(_.map(generic.from)))
}

class OptionalResult[RAW_OUTPUT <: HList](results: IOEither[DataError, Option[RAW_OUTPUT]]) {
  def mapTo[OUTPUT](implicit generic: Generic.Aux[OUTPUT, RAW_OUTPUT]): IOEither[DataError, Option[OUTPUT]] =
    results.map(_.map(_.map(generic.from)))
}

case class RunnableStatement[FIELDS <: HList](sql: String, input: Seq[Value[_]], fields: FIELDS)

trait StatementBuilder[PLACEHOLDERS <: HList, RAW_INPUT <: HList, INPUT, OUTPUT <: HList] {
  def build(query: Query[PLACEHOLDERS, OUTPUT]): Statement[RAW_INPUT, INPUT, OUTPUT]
}

object StatementBuilder {
  implicit def noPlaceholders[OUTPUT <: HList]: StatementBuilder[HNil, HNil, Unit, OUTPUT] =
    (query: Query[HNil, OUTPUT]) =>
      new Statement(_ => RunnableStatement(query.sql, Seq.empty, query.fields))

  implicit def placeholders[P <: Placeholder[_, _] with Tag[_], PTYPE, PTAG <: String, TAIL <: HList, TAIL_INPUT <: HList, OUTPUT <: HList, INPUT_TUPLE]
      (implicit
       tagged: Tagged[P, PTAG],
       fieldT: FieldT[P, PTYPE],
       tail: StatementBuilder[TAIL, TAIL_INPUT, _, OUTPUT],
       tupler: Tupler.Aux[Value[PTYPE] with Tag[PTAG] :: TAIL_INPUT, INPUT_TUPLE],
       generic: Generic.Aux[INPUT_TUPLE, Value[PTYPE] with Tag[PTAG] :: TAIL_INPUT]
      ): StatementBuilder[P :: TAIL, Value[PTYPE] with Tag[PTAG] :: TAIL_INPUT, INPUT_TUPLE, OUTPUT] =
    (query: Query[P :: TAIL, OUTPUT]) =>
      new Statement(values => RunnableStatement(
          query.sql,
          tail
            .build(Query(query.sql, query.placeholders.tail, query.fields))
            .toRunnable(values.tail)
            .input prepended values.head,
          query.fields
        )
      )
}
