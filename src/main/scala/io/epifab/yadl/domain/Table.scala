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
  def `*`: Writer[S]

  def column[T](name: String)(implicit adapter: FieldAdapter[T]): Column[T] =
    Column[T](name, this)
}

trait TableProjection[T, P] extends DataSource {
  def table: Table[T]
  def `*`: Reader[P]
}

case class Relation[+T <: DataSource](dataSource: T, clause: Filter)

trait SubQuery[S, T] extends DataSource {
  def select: Select[T]
  def `*`: Reader[S]

  def field[U](field: Field[U]): SubQueryField[U, S, T] =
    SubQueryField(field, this)
}
