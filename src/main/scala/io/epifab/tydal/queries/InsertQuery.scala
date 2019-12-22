package io.epifab.tydal.queries

import io.epifab.tydal.{As, Tagging}
import io.epifab.tydal.runtime.{StatementBuilder, WriteStatement}
import io.epifab.tydal.schema.{Columns, Field, GenericSchema, NamedPlaceholders, Table, TableBuilder}
import io.epifab.tydal.utils.{TaggedFind, TaggedReplace}
import shapeless.ops.hlist.Tupler
import shapeless.{HList, HNil}

final class InsertQuery[Fields <: HList, Values <: HList](private [tydal] val table: Table[Fields], private [tydal] val values: Values) {
  def set[F <: Field[_] with Tagging[_], A <: String with Singleton, T, NewValues <: HList](field: F As A)(
    implicit
    taggedFind: TaggedFind[A, Field[T], Values],
    taggedReplace: TaggedReplace[A, F, Values, NewValues]
  ): InsertQuery[Fields, NewValues] = new InsertQuery(table, taggedReplace(values, field))

  def compile[Placeholders <: HList, InputRepr <: HList](
    implicit
    queryBuilder: QueryBuilder[this.type, Placeholders, HNil],
    statementBuilder: StatementBuilder[Placeholders, InputRepr, HNil]
  ): WriteStatement[InputRepr, HNil] =
    statementBuilder.build(queryBuilder.build(this)).update
}

object Insert {
  def into[TableName <: String with Singleton, Schema, Fields <: HList, Values <: HList](
    tableBuilder: TableBuilder[TableName, Schema])(
    implicit
    name: ValueOf[TableName],
    genericSchema: GenericSchema.Aux[Schema, Fields],
    placeholders: NamedPlaceholders[Fields, Values],
    column: Columns[Fields]
  ): InsertQuery[Fields, Values] =
    new InsertQuery(tableBuilder as name.value, placeholders.get)
}
