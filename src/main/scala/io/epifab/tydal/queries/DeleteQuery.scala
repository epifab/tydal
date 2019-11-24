package io.epifab.tydal.queries

import io.epifab.tydal.schema._
import io.epifab.tydal.runtime.{StatementBuilder, WriteStatement}
import shapeless.ops.hlist.Tupler
import shapeless.{HList, HNil}

final class DeleteQuery[Fields <: HList, E <: BinaryExpr](val table: Table[Fields], val filter: E) {
  def where[E2 <: BinaryExpr](f: Selectable[Fields] => E2): DeleteQuery[Fields, E2] =
    new DeleteQuery(table, f(table))

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
    ): DeleteQuery[Fields, AlwaysTrue] =
    new DeleteQuery(tableBuilder as name.value, AlwaysTrue)
}
