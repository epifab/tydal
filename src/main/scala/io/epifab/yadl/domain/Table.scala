package io.epifab.yadl.domain

case class Table(src: String, alias: String) extends DataSource {
  def field[T](name: String)(implicit fieldExtractor: FieldExtractor[T]): TableField[T] = TableField[T](name, this)

  def relation(tableName: String, relationName: String, clause: Filter): Table with Relation =
    new Table(tableName, s"${alias}__$relationName") with Relation {
      override def relationClause: Filter = clause
    }
}

object Table {
  def apply(name: String): Table = new Table(name, name)
}

trait Relation {
  def relationClause: Filter
}