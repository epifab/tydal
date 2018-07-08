package io.epifab.yadl.domain

trait Table extends DataSource {
  def alias: String
  def src: String

  def column[T](name: String)(implicit fieldAdapter: FieldAdapter[T]): Column[T] =
    Column[T](name, this)
}

trait Relation {
  def relationClause: Filter
}