package io.epifab.tydal.queries

import io.epifab.tydal.schema._
import shapeless.HList

final class DeleteQuery[Fields <: HList, E <: Filter](val table: Table[Fields], val filter: E) extends WriteQueryBuilder {

  def where[E2 <: Filter](f: Selectable[Fields] => E2): DeleteQuery[Fields, E2] =
    new DeleteQuery(table, f(table))

}

object Delete {
  def from[TableName <: String with Singleton, Schema, Fields <: HList]
    (tableBuilder: TableBuilder[TableName, Schema])
    (implicit
     name: ValueOf[TableName],
     genericSchema: GenericSchema.Aux[Schema, Fields],
     columns: Columns[Fields]
    ): DeleteQuery[Fields, AlwaysTrue] =
    new DeleteQuery(tableBuilder as name.value, AlwaysTrue)
}
