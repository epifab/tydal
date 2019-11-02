package io.epifab.yadl

import io.epifab.yadl.fields.{AlwaysTrue, BinaryExpr, ColumnsBuilder}
import io.epifab.yadl.runner.{QueryBuilder, StatementBuilder, UpdateStatement}
import shapeless.ops.hlist.Tupler
import shapeless.{Generic, HList, HNil}

class Update[NAME <: String, SCHEMA, FIELDS <: HList, E <: BinaryExpr]
    (val table: Table[NAME, SCHEMA], val fields: FIELDS, val where: E) {

  def fields[P, F <: HList]
    (f: SCHEMA => P)
    (implicit generic: Generic.Aux[P, F]): Update[NAME, SCHEMA, F, E] =
    new Update(table, generic.to(f(table.schema)), where)

  def where[E2 <: BinaryExpr](f: SCHEMA => E2): Update[NAME, SCHEMA, FIELDS, E2] =
    new Update(table, fields, f(table.schema))

  def compile[PLACEHOLDERS <: HList, RAW_INPUT <: HList, INPUT]
      (implicit
       queryBuilder: QueryBuilder[this.type, PLACEHOLDERS, HNil],
       statementBuilder: StatementBuilder[PLACEHOLDERS, RAW_INPUT, INPUT, HNil],
       tupler: Tupler.Aux[RAW_INPUT, INPUT]
      ): UpdateStatement[INPUT, HNil] =
    statementBuilder.build(queryBuilder.build(this)).update
}

object Update {
  def apply[NAME <: String, SCHEMA, FIELDS <: HList]
      (tableBuilder: TableBuilder[NAME, SCHEMA])
      (implicit
       name: ValueOf[NAME],
       genericSchema: Generic.Aux[SCHEMA, FIELDS],
       columnsBuilder: ColumnsBuilder[FIELDS]
      ): Update[NAME, SCHEMA, FIELDS, AlwaysTrue] =
    new Update(tableBuilder as name.value, columnsBuilder.build(name.value), AlwaysTrue)
}
