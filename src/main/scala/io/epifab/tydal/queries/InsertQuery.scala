package io.epifab.tydal.queries

import io.epifab.tydal.runtime.{StatementBuilder, WriteStatement}
import io.epifab.tydal.schema.{Columns, GenericSchema, Table, TableBuilder}
import shapeless.ops.hlist.Tupler
import shapeless.{HList, HNil}

final class InsertQuery[Fields <: HList](val table: Table[Fields]) {
  def compile[Placeholders <: HList, InputRepr <: HList, Input]
      (implicit
       queryBuilder: QueryBuilder[this.type, Placeholders, HNil],
       statementBuilder: StatementBuilder[Placeholders, InputRepr, Input, HNil],
       tupler: Tupler.Aux[InputRepr, Input]
      ): WriteStatement[Input, HNil] =
    statementBuilder.build(queryBuilder.build(this)).update
}

object Insert {
  def into[TableName <: String with Singleton, Schema, Fields <: HList]
      (tableBuilder: TableBuilder[TableName, Schema])
      (implicit
       name: ValueOf[TableName],
       genericSchema: GenericSchema.Aux[Schema, Fields],
       column: Columns[Fields]
      ): InsertQuery[Fields] =
    new InsertQuery(tableBuilder as name.value)
}
