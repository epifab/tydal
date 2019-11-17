package io.epifab.tydal

import io.epifab.tydal.fields.{AlwaysTrue, BinaryExpr}
import io.epifab.tydal.runner.{QueryBuilder, StatementBuilder, WriteStatement}
import shapeless.ops.hlist.Tupler
import shapeless.{HList, HNil}

class Delete[NAME <: Tag, FIELDS <: HList, E <: BinaryExpr](val table: Table[NAME, FIELDS], val filter: E) {
  def where[E2 <: BinaryExpr](f: Selectable[FIELDS] => E2): Delete[NAME, FIELDS, E2] =
    new Delete(table, f(table))

  def compile[PLACEHOLDERS <: HList, RAW_INPUT <: HList, INPUT]
      (implicit
       queryBuilder: QueryBuilder[this.type, PLACEHOLDERS, HNil],
       statementBuilder: StatementBuilder[PLACEHOLDERS, RAW_INPUT, INPUT, HNil],
       tupler: Tupler.Aux[RAW_INPUT, INPUT]
      ): WriteStatement[INPUT, HNil] =
    statementBuilder.build(queryBuilder.build(this)).update
}

object Delete {
  def from[NAME <: Tag, SCHEMA, FIELDS <: HList]
    (tableBuilder: TableBuilder[NAME, SCHEMA])
    (implicit
     name: ValueOf[NAME],
     schemaBuilder: SchemaBuilder[SCHEMA, FIELDS]
    ): Delete[NAME, FIELDS, AlwaysTrue] =
    new Delete(tableBuilder as name.value, AlwaysTrue)
}
