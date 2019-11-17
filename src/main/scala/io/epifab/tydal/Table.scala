package io.epifab.tydal

import io.epifab.tydal.utils.TaggedFinder
import shapeless.HList
import shapeless.ops.hlist.Tupler

class TableBuilder[NAME <: Tag, SCHEMA](implicit name: ValueOf[NAME]) {
  def as[ALIAS <: Tag, REPR <: HList](alias: ALIAS)(implicit a: ValueOf[ALIAS], schemaBuilder: SchemaBuilder[SCHEMA, REPR]): Table[NAME, REPR] with Tagging[ALIAS] =
    Table(name.value, schemaBuilder.build(alias), alias)
}

class Table[NAME <: Tag, FIELDS <: HList] private(val tableName: String, override val schema: FIELDS) extends Selectable[FIELDS] with FindContext[FIELDS] { self: Tagging[_] =>
  override def apply[T <: Tag, X](tag: T)(implicit finder: TaggedFinder[T, X, FIELDS]): X with Tagging[T] =
    finder.find(schema)

  def `*`[T](implicit tupler: Tupler.Aux[FIELDS, T]): T = tupler(schema)
}

object Table {
  def unapply(table: Table[_, _]): Option[String] = Some(table.tableName)

  protected[tydal] def apply[NAME <: Tag, FIELDS <: HList, ALIAS <: Tag](tableName: String, fields: FIELDS, tableAlias: String): Table[NAME, FIELDS] with Tagging[ALIAS] = new Table[NAME, FIELDS](tableName, fields) with Tagging[ALIAS] {
    override def tagValue: String = tableAlias
  }
}
