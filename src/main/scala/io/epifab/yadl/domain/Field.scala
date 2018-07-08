package io.epifab.yadl.domain

import scala.language.implicitConversions

abstract class Field[T](override val src: String, override val alias: String, val adapter: FieldAdapter[T]) extends DataSource

case class Column[T](name: String, table: Table)(implicit adapter: FieldAdapter[T])
  extends Field[T](
    s"${table.alias}.$name",
    s"${table.alias}__$name",
    adapter
  )

case class Aggregation[T, U](field: Field[T], dbFunction: Grouping[T, U])(implicit adapter: FieldAdapter[U])
  extends Field[U](
    s"${dbFunction.name}(${field.src})",
    s"${field.alias}_${dbFunction.name}",
    adapter
  )

object Field {
  def apply[T](name: String, dataSource: Table)(implicit adapter: FieldAdapter[T]): Column[T] =
    Column(name, dataSource)

  def apply[T, U](field: Field[T], dbFunction: Grouping[T, U])(implicit adapter: FieldAdapter[U]): Aggregation[T, U] =
    Aggregation(field, dbFunction)
}

trait Value[T] {
  type U
  def adapter: FieldAdapter.Aux[T, U]
  def value: T
  lazy val dbValue: U = adapter.toDb(value)

  override def equals(obj: scala.Any): Boolean = obj match {
    case t: Value[T] => t.value == value
    case _ => false
  }
}

object Value {
  def apply[T](v: T)(implicit a: FieldAdapter[T]): Value[T] = new Value[T] {
    type U = a.DBTYPE
    override def adapter: FieldAdapter.Aux[T, U] = a
    override def value: T = v
  }
}

trait ColumnValue[T] extends Value[T] {
  def column: Column[T]
  def value: T
}

object ColumnValue {
  implicit def apply[T](fieldValue: (Column[T], T)): ColumnValue[T] = new ColumnValue[T] {
    override type U = fieldValue._1.adapter.DBTYPE
    override val adapter: FieldAdapter.Aux[T, U] = fieldValue._1.adapter

    override val column: Column[T] = fieldValue._1
    override val value: T = fieldValue._2
  }
}
