package io.epifab.yadl.runner

import java.sql.Connection

import io.epifab.yadl._
import io.epifab.yadl.fields.{FieldT, NamedPlaceholder, OptionalPlaceholderValue, PlaceholderValue}
import shapeless.ops.hlist.Tupler
import shapeless.{::, Generic, HList, HNil}

class GenericStatement[RAW_INPUT <: HList, INPUT, FIELDS <: HList]
(val query: String, val toRunnable: RAW_INPUT => RunnableStatement[FIELDS])
(implicit tupler: Tupler.Aux[RAW_INPUT, INPUT], generic: Generic.Aux[INPUT, RAW_INPUT]) {
  def update: UpdateStatement[INPUT, FIELDS] =
    new UpdateStatement(query, (generic.to _).andThen(toRunnable))
  def select: SelectStatement[INPUT, FIELDS] =
    new SelectStatement(query, (generic.to _).andThen(toRunnable))
}

class UpdateStatement[INPUT, FIELDS <: HList]
(val query: String, toRunnable: INPUT => RunnableStatement[FIELDS]) {
  def withValues
  (values: INPUT)
  (implicit statementExecutor: StatementExecutor[IOEither, Connection, FIELDS, Int]):
  TransactionIO[Int] = TransactionIO(toRunnable(values))
}

class SelectStatement[INPUT, FIELDS <: HList]
(val query: String, toRunnable: INPUT => RunnableStatement[FIELDS]) {
  def withValues[RAW_OUTPUT <: HList]
  (values: INPUT)
  (implicit statementExecutor: StatementExecutor[IOEither, Connection, FIELDS, Seq[RAW_OUTPUT]]):
  TransactionIO[Seq[RAW_OUTPUT]] = TransactionIO(toRunnable(values))
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

  implicit def optionalValue[P <: OptionalPlaceholderValue[_], PTYPE, TAIL <: HList, TAIL_INPUT <: HList, OUTPUT <: HList, INPUT_TUPLE]
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
