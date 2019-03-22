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

abstract class View[S](val _name_ : String) extends DataSource {
  def `*`: Terms[S]
}

abstract class Table[S](sourceName: String) extends View[S](sourceName) {
  def `*`: Columns[S]

  def column[T](name: String)(implicit adapter: FieldAdapter[T]): Column[T] =
    Column[T](name, this)
}

case class Relation[+T <: DataSource](dataSource: T, clause: BinaryExpr)

trait SubQuery[S, T] extends DataSource {
  def select: Select[T]
  def `*`: Terms[S]

  def term[U](term: Term[U]): SubQueryTerm[U, S, T] =
    SubQueryTerm(term, this)
}
