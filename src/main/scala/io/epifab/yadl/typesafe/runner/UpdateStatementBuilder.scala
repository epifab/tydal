package io.epifab.yadl.typesafe.runner

import io.epifab.yadl.typesafe.{DataError, IOEither, Query, Tag, Tagged}
import io.epifab.yadl.typesafe.fields.{FieldT, Placeholder, Value}
import shapeless.{::, Generic, HList, HNil}
import shapeless.ops.hlist.Tupler

class UpdateStatement[RAW_INPUT <: HList, INPUT](val toRunnable: RAW_INPUT => RunnableUpdateStatement)(implicit tupler: Tupler.Aux[RAW_INPUT, INPUT], generic: Generic.Aux[INPUT, RAW_INPUT]) {
  def run[CONN, RAW_OUTPUT <: HList](connection: CONN)(values: INPUT)(implicit statementExecutor: UpdateStatementExecutor[IOEither, CONN]): IOEither[DataError, Int] =
    statementExecutor.run(connection, toRunnable(generic.to(values)))
}

case class RunnableUpdateStatement(sql: String, input: Seq[Value[_]])
  extends RunnableStatement

trait UpdateStatementBuilder[PLACEHOLDERS <: HList, RAW_INPUT <: HList, INPUT] {
  def build(query: Query[PLACEHOLDERS, HNil]): UpdateStatement[RAW_INPUT, INPUT]
}

object UpdateStatementBuilder {
  implicit def noPlaceholders: UpdateStatementBuilder[HNil, HNil, Unit] =
    (query: Query[HNil, HNil]) =>
      new UpdateStatement(_ => RunnableUpdateStatement(query.sql, Seq.empty))

  implicit def placeholders[P <: Placeholder[_, _] with Tag[_], PTYPE, PTAG <: String, TAIL <: HList, TAIL_INPUT <: HList, INPUT_TUPLE]
    (implicit
     tagged: Tagged[P, PTAG],
     fieldT: FieldT[P, PTYPE],
     tail: UpdateStatementBuilder[TAIL, TAIL_INPUT, _],
     tupler: Tupler.Aux[Value[PTYPE] with Tag[PTAG] :: TAIL_INPUT, INPUT_TUPLE],
     generic: Generic.Aux[INPUT_TUPLE, Value[PTYPE] with Tag[PTAG] :: TAIL_INPUT]
    ): UpdateStatementBuilder[P :: TAIL, Value[PTYPE] with Tag[PTAG] :: TAIL_INPUT, INPUT_TUPLE] =
    (query: Query[P :: TAIL, HNil]) =>
      new UpdateStatement(values => RunnableUpdateStatement(
        query.sql,
        tail
          .build(Query(query.sql, query.placeholders.tail, query.fields))
          .toRunnable(values.tail)
          .input prepended values.head
      ))
}
