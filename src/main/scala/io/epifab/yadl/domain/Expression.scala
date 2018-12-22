package io.epifab.yadl.domain

import scala.language.implicitConversions


trait Expression[T] {
  def adapter: FieldAdapter[T]
}

sealed trait Column[T] extends Expression[T] {
  def value(t: T): Value[T] = Value(t)(adapter)
}

final case class TableColumn[T](name: String, table: Table)(implicit val adapter: FieldAdapter[T])
  extends Column[T]

final case class AggregateColumn[T, U](column: Column[T], dbFunction: AggregateFunction[T, U])(implicit val adapter: FieldAdapter[U])
  extends Column[U]

final case class SubQueryColumn[T](column: Column[T], subQuery: SubQuery) extends Column[T] {
  override def adapter: FieldAdapter[T] = column.adapter
}

object Column {
  implicit class StringColumn(val value: Column[String])
  implicit class IntColumn(val value: Column[Int])
  implicit class DoubleColumn(val value: Column[Double])

  implicit class SeqStringColumn(val value: Column[Seq[String]])
  implicit class SeqIntColumn(val value: Column[Seq[String]])
  implicit class SeqDoubleColumn(val value: Column[Seq[String]])

  implicit class OptionalStringColumn(val value: Column[Option[String]])
  implicit class OptionalIntColumn(val value: Column[Option[Int]])
  implicit class OptionalDoubleColumn(val value: Column[Option[Double]])

  implicit class OptionalSeqStringColumn(val value: Column[Option[Seq[String]]])
  implicit class OptionalSeqIntColumn(val value: Column[Option[Seq[String]]])
  implicit class OptionalSeqDoubleColumn(val value: Column[Option[Seq[String]]])

  def apply[T](name: String, dataSource: Table)(implicit adapter: FieldAdapter[T]): TableColumn[T] =
    TableColumn(name, dataSource)

  def apply[T, U](column: Column[T], dbFunction: AggregateFunction[T, U])(implicit adapter: FieldAdapter[U]): AggregateColumn[T, U] =
    AggregateColumn(column, dbFunction)
}

final case class Value[T](value: T)(implicit val adapter: FieldAdapter[T]) extends Expression[T] {
  lazy val dbValue: adapter.DBTYPE = adapter.toDb(value)

  override def equals(obj: scala.Any): Boolean = obj match {
    case t: Value[T] => t.value == value
    case _ => false
  }
}

case class ColumnValue[T](column: TableColumn[T], value: Value[T])

object ColumnValue {
  implicit def apply[T](columnValue: (TableColumn[T], T)): ColumnValue[T] =
    ColumnValue[T](columnValue._1, Value(columnValue._2)(columnValue._1.adapter))
}
