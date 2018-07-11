package io.epifab.yadl.domain

trait Table {
  def tableAlias: String
  def tableName: String

  def column[T](name: String)(implicit fieldAdapter: FieldAdapter[T]): TableColumn[T] =
    TableColumn[T](name, this)
}

trait Relation {
  def relationClause: Filter
}