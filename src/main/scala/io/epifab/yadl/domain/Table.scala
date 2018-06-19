package io.epifab.yadl.domain

trait Table extends DataSource {
  def alias: String
  def src: String

  def field[T, U](name: String)(implicit fieldAdapter: FieldAdapter[T, U]): TableField[T, U] =
    TableField[T, U](name, this)
}

trait Relation {
  def relationClause: Filter
}