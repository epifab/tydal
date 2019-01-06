package io.epifab.yadl.domain

import scala.language.implicitConversions


trait Expression[T] {
  def adapter: FieldAdapter[T]
}

sealed trait Field[T] extends Expression[T] {
  def value(t: T): Value[T] = Value(t)(adapter)
}

final case class Column[T](name: String, table: Table[_])(implicit val adapter: FieldAdapter[T])
  extends Field[T]

final case class Aggregation[T, U](field: Field[T], dbFunction: AggregateFunction[T, U])(implicit val adapter: FieldAdapter[U])
  extends Field[U]

final case class SubQueryField[U, S, T](field: Field[U], subQuery: SubQuery[S, T]) extends Field[U] {
  override def adapter: FieldAdapter[U] = field.adapter
}

object Field {
  implicit class StringField(val value: Field[String])
  implicit class IntField(val value: Field[Int])
  implicit class DoubleField(val value: Field[Double])

  implicit class SeqStringField(val value: Field[Seq[String]])
  implicit class SeqIntField(val value: Field[Seq[String]])
  implicit class SeqDoubleField(val value: Field[Seq[String]])

  implicit class OptionalStringField(val value: Field[Option[String]])
  implicit class OptionalIntField(val value: Field[Option[Int]])
  implicit class OptionalDoubleField(val value: Field[Option[Double]])

  implicit class OptionalSeqStringField(val value: Field[Option[Seq[String]]])
  implicit class OptionalSeqIntField(val value: Field[Option[Seq[String]]])
  implicit class OptionalSeqDoubleField(val value: Field[Option[Seq[String]]])

  def apply[T](name: String, dataSource: Table[_])(implicit adapter: FieldAdapter[T]): Column[T] =
    Column(name, dataSource)

  def apply[T, U](field: Field[T], dbFunction: AggregateFunction[T, U])(implicit adapter: FieldAdapter[U]): Aggregation[T, U] =
    Aggregation(field, dbFunction)
}

final case class Value[T](value: T)(implicit val adapter: FieldAdapter[T]) extends Expression[T] {
  lazy val dbValue: adapter.DBTYPE = adapter.toDb(value)

  override def equals(obj: scala.Any): Boolean = obj match {
    case t: Value[T] => t.value == value
    case _ => false
  }
}

case class ColumnValue[T](column: Column[T], value: Value[T])

object ColumnValue {
  implicit def apply[T](columnValue: (Column[T], T)): ColumnValue[T] =
    ColumnValue[T](columnValue._1, Value(columnValue._2)(columnValue._1.adapter))
}
