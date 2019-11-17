package io.epifab.tydal

import io.epifab.tydal.fields.{AlwaysTrue, BinaryExpr}
import io.epifab.tydal.runner.{QueryBuilder, StatementBuilder, WriteStatement}
import shapeless.ops.hlist.Tupler
import shapeless.{HList, HNil}

class Delete[TableName <: String with Singleton, Fields <: HList, E <: BinaryExpr](val table: Table[TableName, Fields], val filter: E) {
  def where[E2 <: BinaryExpr](f: Selectable[Fields] => E2): Delete[TableName, Fields, E2] =
    new Delete(table, f(table))

  def compile[Placeholders <: HList, InputRepr <: HList, Input]
      (implicit
       queryBuilder: QueryBuilder[this.type, Placeholders, HNil],
       statementBuilder: StatementBuilder[Placeholders, InputRepr, Input, HNil],
       tupler: Tupler.Aux[InputRepr, Input]
      ): WriteStatement[Input, HNil] =
    statementBuilder.build(queryBuilder.build(this)).update
}

object Delete {
  def from[TableName <: String with Singleton, Schema, Fields <: HList]
    (tableBuilder: TableBuilder[TableName, Schema])
    (implicit
     name: ValueOf[TableName],
     schemaBuilder: SchemaBuilder[Schema, Fields]
    ): Delete[TableName, Fields, AlwaysTrue] =
    new Delete(tableBuilder as name.value, AlwaysTrue)
}
