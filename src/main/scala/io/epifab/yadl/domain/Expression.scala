package io.epifab.yadl.domain

import scala.language.implicitConversions


trait Expression[T] {
  def adapter: FieldAdapter[T]
}

sealed trait Field[T] extends Expression[T] {
  def value(t: T): Value[T] = Value(t)(adapter)
}

final case class TableColumn[T](name: String, table: Table[_])(implicit val adapter: FieldAdapter[T])
  extends Field[T]

final case class AggregateColumn[T, U](column: Field[T], dbFunction: AggregateFunction[T, U])(implicit val adapter: FieldAdapter[U])
  extends Field[U]

final case class SubQueryColumn[U, S, T](column: Field[U], subQuery: SubQuery[S, T]) extends Field[U] {
  override def adapter: FieldAdapter[U] = column.adapter
}

object Field {
  implicit class StringColumn(val value: Field[String])
  implicit class IntColumn(val value: Field[Int])
  implicit class DoubleColumn(val value: Field[Double])

  implicit class SeqStringColumn(val value: Field[Seq[String]])
  implicit class SeqIntColumn(val value: Field[Seq[String]])
  implicit class SeqDoubleColumn(val value: Field[Seq[String]])

  implicit class OptionalStringColumn(val value: Field[Option[String]])
  implicit class OptionalIntColumn(val value: Field[Option[Int]])
  implicit class OptionalDoubleColumn(val value: Field[Option[Double]])

  implicit class OptionalSeqStringColumn(val value: Field[Option[Seq[String]]])
  implicit class OptionalSeqIntColumn(val value: Field[Option[Seq[String]]])
  implicit class OptionalSeqDoubleColumn(val value: Field[Option[Seq[String]]])

  def apply[T](name: String, dataSource: Table[_])(implicit adapter: FieldAdapter[T]): TableColumn[T] =
    TableColumn(name, dataSource)

  def apply[T, U](column: Field[T], dbFunction: AggregateFunction[T, U])(implicit adapter: FieldAdapter[U]): AggregateColumn[T, U] =
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
