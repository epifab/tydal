package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields.ColumnsBuilder
import io.epifab.yadl.typesafe.utils.TaggedFinder
import shapeless.Generic

class TableBuilder[NAME <: String, SCHEMA](implicit name: ValueOf[NAME]) {
  def as[ALIAS <: Singleton with String, REPR](alias: ALIAS)(implicit a: ValueOf[ALIAS], generic: Generic.Aux[SCHEMA, REPR], columnsBuilder: ColumnsBuilder[REPR]): Table[NAME, SCHEMA] with Tag[ALIAS] =
    Table(name.value, generic.from(columnsBuilder.build(alias)), alias)
}

class Table[NAME <: String, SCHEMA] private(val tableName: String, override val schema: SCHEMA) extends Selectable[SCHEMA] with FindContext[SCHEMA] { self: Tag[_] =>
  override def apply[TAG <: String with Singleton, X](tag: TAG)(implicit finder: TaggedFinder[TAG, X, SCHEMA]): X with Tag[TAG] =
    finder.find(schema)

  def `*`: SCHEMA = schema
}

object Table {
  def unapply(table: Table[_, _]): Option[String] = Some(table.tableName)

  protected[typesafe] def apply[NAME <: String, FIELDS, ALIAS <: String](tableName: String, fields: FIELDS, tableAlias: String): Table[NAME, FIELDS] with Tag[ALIAS] = new Table[NAME, FIELDS](tableName, fields) with Tag[ALIAS] {
    override def tagValue: String = tableAlias
  }
}
