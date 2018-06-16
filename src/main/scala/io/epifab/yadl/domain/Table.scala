package io.epifab.yadl.domain

trait Table extends DataSource {
  def alias: String
  def src: String

  def field[T](name: String)(implicit fieldExtractor: FieldExtractor[T]): TableField[T] =
    TableField[T](name, this)
}

trait Relation {
  def relationClause: Filter
}