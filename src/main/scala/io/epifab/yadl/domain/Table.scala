package io.epifab.yadl.domain

abstract class Table(val tableName: String) {
  def tableAlias: String

  def column[T](name: String)(implicit fieldAdapter: FieldAdapter[T]): TableColumn[T] =
    TableColumn[T](name, this)
}

trait Relation {
  def relationClause: Filter
}
