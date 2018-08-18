package io.epifab.yadl.domain

sealed trait DataSource

abstract class Table(val tableName: String) extends DataSource {
  def column[T](name: String)(implicit adapter: FieldAdapter[T]): TableColumn[T] =
    TableColumn[T](name, this)
}

trait SubQuery extends DataSource {
  def select: Select

  def column[T](column: Column[T]): SubQueryColumn[T] =
    SubQueryColumn[T](column, this)
}


trait Relation {
  def relationClause: Filter
}
