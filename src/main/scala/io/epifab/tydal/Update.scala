package io.epifab.tydal

import io.epifab.tydal.fields.{AlwaysTrue, BinaryExpr}
import io.epifab.tydal.runner.{QueryBuilder, StatementBuilder, WriteStatement}
import shapeless.ops.hlist.Tupler
import shapeless.{Generic, HList, HNil}

class Update[TableName <: String with Singleton, TableFields <: HList, FieldsToUpdate <: HList, Where <: BinaryExpr]
    (val table: Table[TableName, TableFields], val $fields: FieldsToUpdate, val $where: Where) {

  def fields[P, NewFields <: HList]
    (f: Selectable[TableFields] => P)
    (implicit generic: Generic.Aux[P, NewFields]): Update[TableName, TableFields, NewFields, Where] =
    new Update(table, generic.to(f(table)), $where)

  def where[E2 <: BinaryExpr](f: Selectable[TableFields] => E2): Update[TableName, TableFields, FieldsToUpdate, E2] =
    new Update(table, $fields, f(table))

  def compile[Placeholders <: HList, InputRepr <: HList, Input]
      (implicit
       queryBuilder: QueryBuilder[this.type, Placeholders, HNil],
       statementBuilder: StatementBuilder[Placeholders, InputRepr, Input, HNil],
       tupler: Tupler.Aux[InputRepr, Input]
      ): WriteStatement[Input, HNil] =
    statementBuilder.build(queryBuilder.build(this)).update
}

object Update {
  def apply[TableName <: String with Singleton, Schema, Fields <: HList]
      (tableBuilder: TableBuilder[TableName, Schema])
      (implicit
       name: ValueOf[TableName],
       schemaBuilder: SchemaBuilder[Schema, Fields]
      ): Update[TableName, Fields, Fields, AlwaysTrue] =
    new Update(tableBuilder as name.value, schemaBuilder.build(name.value), AlwaysTrue)
}
