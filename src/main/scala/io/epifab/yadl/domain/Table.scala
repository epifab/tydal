package io.epifab.yadl.domain

import scala.language.implicitConversions

sealed trait DataSource {
  def on(filter: Filter): Relation[this.type] = Relation(this, filter)

  def on(f: this.type => Filter): Relation[this.type] = Relation(this, f(this))
}

object DataSource {
  implicit def fromRelation[T <: DataSource](relation: Relation[T]): T = relation.dataSource
}

abstract class Table(val tableName: String) extends DataSource {
  def column[T](name: String)(implicit adapter: FieldAdapter[T]): TableColumn[T] =
    TableColumn[T](name, this)
}

case class Relation[+T <: DataSource](dataSource: T, clause: Filter)

trait SubQuery extends DataSource {
  def select: Select

  def column[T](column: Column[T]): SubQueryColumn[T] =
    SubQueryColumn[T](column, this)
}
