package io.epifab.yadl.domain

import scala.language.implicitConversions

sealed trait DataSource {
  def on(filter: Filter): Relation[this.type] = Relation(this, filter)

  def on(f: this.type => Filter): Relation[this.type] = Relation(this, f(this))
}

object DataSource {
  implicit def fromRelation[T <: DataSource](relation: Relation[T]): T = relation.dataSource
}

abstract class Table[S](val tableName: String) extends DataSource {
  def `*`: Selectable[S]

  def column[T](name: String)(implicit adapter: FieldAdapter[T]): TableColumn[T] =
    TableColumn[T](name, this)
}

trait TableProjection[T, P] extends DataSource {
  def table: Table[T]
  def `*`: Selectable[P]
}

case class Relation[+T <: DataSource](dataSource: T, clause: Filter)

trait SubQuery[S, T] extends DataSource {
  def select: Select[T]
  def `*`: Selectable[S]

  def column[U](column: Column[U]): SubQueryColumn[U, S, T] =
    SubQueryColumn(column, this)
}
