package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields.{BinaryExpr, ColumnsBuilder}
import io.epifab.yadl.typesafe.runner.{UpdateStatement, UpdateStatementBuilder}
import shapeless.ops.hlist.Tupler
import shapeless.{Generic, HList, HNil}

class UnfilteredUpdate[NAME <: String, SCHEMA, FIELDS <: HList](val table: Table[NAME, SCHEMA], val fields: FIELDS) {
  def fields[P, F <: HList]
      (f: SCHEMA => P)
      (implicit generic: Generic.Aux[P, F]): UnfilteredUpdate[NAME, SCHEMA, F] =
    new UnfilteredUpdate(table, generic.to(f(table.schema)))

  def where[E <: BinaryExpr](f: SCHEMA => E): Update[NAME, SCHEMA, FIELDS, E] =
    new Update(table, fields, f(table.schema))
}

class Update[NAME <: String, SCHEMA, FIELDS <: HList, E <: BinaryExpr]
    (val table: Table[NAME, SCHEMA], val fields: FIELDS, val where: E) {

  def compile[PLACEHOLDERS <: HList, RAW_INPUT <: HList, INPUT]
      (implicit
       queryBuilder: QueryBuilder[this.type, PLACEHOLDERS, HNil],
       statementBuilder: UpdateStatementBuilder[PLACEHOLDERS, RAW_INPUT, INPUT],
       tupler: Tupler.Aux[RAW_INPUT, INPUT]
      ): UpdateStatement[RAW_INPUT, INPUT] =
    statementBuilder.build(queryBuilder.build(this))
}

object Update {
  def apply[NAME <: String, SCHEMA, FIELDS <: HList]
      (tableBuilder: TableBuilder[NAME, SCHEMA])
      (implicit
       name: ValueOf[NAME],
       genericSchema: Generic.Aux[SCHEMA, FIELDS],
       columnsBuilder: ColumnsBuilder[FIELDS]
      ): UnfilteredUpdate[NAME, SCHEMA, FIELDS] =
    new UnfilteredUpdate(tableBuilder as name.value, columnsBuilder.build(name.value))
}
