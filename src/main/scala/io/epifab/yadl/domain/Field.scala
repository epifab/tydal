package io.epifab.yadl.domain

import scala.language.implicitConversions

trait Field[T] extends DataSource {
  def adapter: FieldAdapter[T]
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

case class TableField[T](name: String, dataSource: Table)(implicit val adapter: FieldAdapter[T]) extends Field[T] {
  override def src: String = s"${dataSource.alias}.$name"
  override def alias: String = s"${dataSource.alias}__$name"
}

trait FieldValue[T] extends Value[T] {
  def field: TableField[T]
  def value: T
}

object FieldValue {
  implicit def apply[T](fieldValue: (TableField[T], T)): FieldValue[T] = new FieldValue[T] {
    override type U = fieldValue._1.adapter.DBTYPE
    override val adapter: FieldAdapter.Aux[T, U] = fieldValue._1.adapter

    override val field: TableField[T] = fieldValue._1
    override val value: T = fieldValue._2
  }
}
