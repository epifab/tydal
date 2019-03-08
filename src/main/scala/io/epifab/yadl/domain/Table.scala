package io.epifab.yadl.domain

import shapeless.HList

import scala.language.implicitConversions

sealed trait DataSource {
  def on(filter: BinaryExpr): Relation[this.type] = Relation(this, filter)

  def on(f: this.type => BinaryExpr): Relation[this.type] = Relation(this, f(this))
}

object DataSource {
  implicit def fromRelation[T <: DataSource](relation: Relation[T]): T = relation.dataSource
}

abstract class Table[S](val tableName: String) extends DataSource {
  def `*`: Columns[S]

  def column[T](name: String)(implicit adapter: FieldAdapter[T]): Column[T] =
    Column[T](name, this)
}

trait TableProjection[T, P] extends DataSource {
  def table: Table[T]
  def `*`: Terms[P]
}

case class Relation[+T <: DataSource](dataSource: T, clause: BinaryExpr)

trait SubQuery[S, T] extends DataSource {
  def select: Select[T]
  def `*`: Terms[S]

  def term[U](term: Term[U]): SubQueryTerm[U, S, T] =
    SubQueryTerm(term, this)
}
