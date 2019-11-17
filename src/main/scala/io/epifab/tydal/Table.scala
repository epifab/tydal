package io.epifab.tydal

import io.epifab.tydal.utils.TaggedFinder
import shapeless.HList
import shapeless.ops.hlist.Tupler

class TableBuilder[TableName <: String with Singleton, Schema](implicit name: ValueOf[TableName]) {
  def as[Alias <: String with Singleton, Repr <: HList](alias: Alias)(implicit a: ValueOf[Alias], schemaBuilder: SchemaBuilder[Schema, Repr]): Table[TableName, Repr] with Tagging[Alias] =
    Table(name.value, schemaBuilder.build(alias), alias)
}

class Table[TableName <: String with Singleton, Fields <: HList] private(val tableName: String, override val rightFields: Fields) extends Selectable[Fields] with FindContext[Fields] { self: Tagging[_] =>
  override def apply[T <: String with Singleton, X](tag: T)(implicit finder: TaggedFinder[T, X, Fields]): X with Tagging[T] =
    finder.find(rightFields)

  def `*`[T](implicit tupler: Tupler.Aux[Fields, T]): T = tupler(rightFields)
}

object Table {
  def unapply(table: Table[_, _]): Option[String] = Some(table.tableName)

  protected[tydal] def apply[TableName <: String with Singleton, Fields <: HList, Alias <: String with Singleton](tableName: String, fields: Fields, tableAlias: String): Table[TableName, Fields] with Tagging[Alias] = new Table[TableName, Fields](tableName, fields) with Tagging[Alias] {
    override def tagValue: String = tableAlias
  }
}
