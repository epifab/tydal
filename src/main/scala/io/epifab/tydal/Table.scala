package io.epifab.tydal

import io.epifab.tydal.fields.ColumnsBuilder
import io.epifab.tydal.utils.TaggedFinder
import shapeless.Generic

class TableBuilder[NAME <: Tag, SCHEMA](implicit name: ValueOf[NAME]) {
  def as[ALIAS <: Singleton with String, REPR](alias: ALIAS)(implicit a: ValueOf[ALIAS], generic: Generic.Aux[SCHEMA, REPR], columnsBuilder: ColumnsBuilder[REPR]): Table[NAME, SCHEMA] with Tagging[ALIAS] =
    Table(name.value, generic.from(columnsBuilder.build(alias)), alias)
}

class Table[NAME <: Tag, SCHEMA] private(val tableName: String, override val schema: SCHEMA) extends Selectable[SCHEMA] with FindContext[SCHEMA] { self: Tagging[_] =>
  override def apply[T <: Tag, X](tag: T)(implicit finder: TaggedFinder[T, X, SCHEMA]): X with Tagging[T] =
    finder.find(schema)

  def `*`: SCHEMA = schema
}

object Table {
  def unapply(table: Table[_, _]): Option[String] = Some(table.tableName)

  protected[tydal] def apply[NAME <: Tag, FIELDS, ALIAS <: Tag](tableName: String, fields: FIELDS, tableAlias: String): Table[NAME, FIELDS] with Tagging[ALIAS] = new Table[NAME, FIELDS](tableName, fields) with Tagging[ALIAS] {
    override def tagValue: String = tableAlias
  }
}
