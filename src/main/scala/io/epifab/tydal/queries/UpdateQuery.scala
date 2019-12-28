package io.epifab.tydal.queries

import io.epifab.tydal.{As, Tagging}
import io.epifab.tydal.runtime.{StatementBuilder, WriteStatement}
import io.epifab.tydal.schema.{AlwaysTrue, Columns, Field, Filter, GenericSchema, NamedPlaceholders, Table, TableBuilder}
import io.epifab.tydal.utils.{TaggedFind, TaggedReplace}
import shapeless.ops.hlist.Tupler
import shapeless.{Generic, HList, HNil}

final class UpdateQuery[Fields <: HList, Values <: HList, Where <: Filter](
  private[tydal] val table: Table[Fields],
  private[tydal] val values: Values,
  private[tydal] val where: Where
) extends WriteQueryBuilder {

  def fields[P, NewFields <: HList, NewValues <: HList](
    f: Selectable[Fields] => P)(
    implicit
    generic: Generic.Aux[P, NewFields],
    placeholders: NamedPlaceholders[NewFields, NewValues]
  ): UpdateQuery[Fields, NewValues, Where] =
    new UpdateQuery(table, placeholders.get, where)

  def set[F <: Field[_] with Tagging[_], A <: String with Singleton, T, NewValues <: HList](field: F As A)(
    implicit
    taggedFind: TaggedFind[A, Field[T], Values],
    taggedReplace: TaggedReplace[A, F, Values, NewValues]
  ): UpdateQuery[Fields, NewValues, Where] = new UpdateQuery(table, taggedReplace(values, field), where)

  def where[E2 <: Filter](f: Selectable[Fields] => E2): UpdateQuery[Fields, Values, E2] =
    new UpdateQuery(table, values, f(table))

}

object Update {
  def apply[TableName <: String with Singleton, Schema, Fields <: HList, Values <: HList](
    tableBuilder: TableBuilder[TableName, Schema])(
    implicit
    name: ValueOf[TableName],
    genericSchema: GenericSchema.Aux[Schema, Fields],
    namedPlaceholders: NamedPlaceholders[Fields, Values],
    columns: Columns[Fields]
  ): UpdateQuery[Fields, Values, AlwaysTrue] =
    new UpdateQuery(tableBuilder as name.value, namedPlaceholders.get, AlwaysTrue)
}
