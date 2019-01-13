package io.epifab.yadl.domain

import scala.language.implicitConversions

sealed trait Term[T] {
  def adapter: FieldAdapter[T]
}

final case class Column[T](name: String, table: Table[_])(implicit val adapter: FieldAdapter[T])
  extends Term[T]

final case class Aggregation[T, U](term: Term[T], dbFunction: AggregateFunction[T, U])(implicit val adapter: FieldAdapter[U])
  extends Term[U]

final case class SubQueryTerm[U, S, T](term: Term[U], subQuery: SubQuery[S, T]) extends Term[U] {
  override def adapter: FieldAdapter[U] = term.adapter
}

final case class Value[T](value: T)(implicit val adapter: FieldAdapter[T]) extends Term[T] {
  lazy val dbValue: adapter.DBTYPE = adapter.toDb(value)

  override def equals(obj: scala.Any): Boolean = obj match {
    case t: Value[T] => t.value == value
    case _ => false
  }
}

object Term {
  implicit class StringTerm(val value: Term[String])
  implicit class IntTerm(val value: Term[Int])
  implicit class DoubleTerm(val value: Term[Double])

  implicit class SeqStringTerm(val value: Term[Seq[String]])
  implicit class SeqIntTerm(val value: Term[Seq[String]])
  implicit class SeqDoubleTerm(val value: Term[Seq[String]])

  implicit class OptionalStringTerm(val value: Term[Option[String]])
  implicit class OptionalIntTerm(val value: Term[Option[Int]])
  implicit class OptionalDoubleTerm(val value: Term[Option[Double]])

  implicit class OptionalSeqStringTerm(val value: Term[Option[Seq[String]]])
  implicit class OptionalSeqIntTerm(val value: Term[Option[Seq[String]]])
  implicit class OptionalSeqDoubleTerm(val value: Term[Option[Seq[String]]])

  def apply[T](name: String, dataSource: Table[_])(implicit adapter: FieldAdapter[T]): Column[T] =
    Column(name, dataSource)

  def apply[T, U](term: Term[T], dbFunction: AggregateFunction[T, U])(implicit adapter: FieldAdapter[U]): Aggregation[T, U] =
    Aggregation(term, dbFunction)
}

case class ColumnValue[T](column: Column[T], value: Value[T])

object ColumnValue {
  implicit def apply[T](columnValue: (Column[T], T)): ColumnValue[T] =
    ColumnValue[T](columnValue._1, Value(columnValue._2)(columnValue._1.adapter))
}
