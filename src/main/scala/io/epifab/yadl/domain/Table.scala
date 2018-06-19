package io.epifab.yadl.domain

trait Table extends DataSource {
  def alias: String
  def src: String

  def field[T, U](name: String)(implicit fieldAdapter: FieldAdapter[T, U]): TableField[T] =
    TableField[T](name, this)
}

trait Relation {
  def relationClause: Filter
}