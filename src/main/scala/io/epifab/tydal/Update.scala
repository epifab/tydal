package io.epifab.tydal

import io.epifab.tydal.fields.{AlwaysTrue, BinaryExpr}
import io.epifab.tydal.runner.{QueryBuilder, StatementBuilder, WriteStatement}
import shapeless.ops.hlist.Tupler
import shapeless.{Generic, HList, HNil}

class Update[NAME <: Tag, TABLE_FIELDS <: HList, FIELDS <: HList, WHERE <: BinaryExpr]
    (val table: Table[NAME, TABLE_FIELDS], val $fields: FIELDS, val $where: WHERE) {

  def fields[P, NEW_FIELDS <: HList]
    (f: Selectable[TABLE_FIELDS] => P)
    (implicit generic: Generic.Aux[P, NEW_FIELDS]): Update[NAME, TABLE_FIELDS, NEW_FIELDS, WHERE] =
    new Update(table, generic.to(f(table)), $where)

  def where[E2 <: BinaryExpr](f: Selectable[TABLE_FIELDS] => E2): Update[NAME, TABLE_FIELDS, FIELDS, E2] =
    new Update(table, $fields, f(table))

  def compile[PLACEHOLDERS <: HList, RAW_INPUT <: HList, INPUT]
      (implicit
       queryBuilder: QueryBuilder[this.type, PLACEHOLDERS, HNil],
       statementBuilder: StatementBuilder[PLACEHOLDERS, RAW_INPUT, INPUT, HNil],
       tupler: Tupler.Aux[RAW_INPUT, INPUT]
      ): WriteStatement[INPUT, HNil] =
    statementBuilder.build(queryBuilder.build(this)).update
}

object Update {
  def apply[NAME <: Tag, SCHEMA, FIELDS <: HList]
      (tableBuilder: TableBuilder[NAME, SCHEMA])
      (implicit
       name: ValueOf[NAME],
       schemaBuilder: SchemaBuilder[SCHEMA, FIELDS]
      ): Update[NAME, FIELDS, FIELDS, AlwaysTrue] =
    new Update(tableBuilder as name.value, schemaBuilder.build(name.value), AlwaysTrue)
}
