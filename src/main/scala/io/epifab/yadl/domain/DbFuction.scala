package io.epifab.yadl.domain

import io.epifab.yadl.domain.Field._

import scala.language.implicitConversions

sealed trait DbFuction[T, U]

sealed trait Grouping[T, U] extends DbFuction[T, U] {
  def name: String
}

sealed trait Avg[T] extends Grouping[T, Option[Double]] {
  override val name: String = "avg"
}

object Avg {
  def apply(field: IntField)(implicit adapter: FieldAdapter[Option[Double]]): Aggregation[Int, Option[Double]] =
    Field(field.field, new Avg[Int] {})(adapter)

  def apply(field: DoubleField)(implicit adapter: FieldAdapter[Option[Double]]): Aggregation[Double, Option[Double]] =
    Field(field.field, new Avg[Double] {})(adapter)
}

sealed trait Sum[T] extends Grouping[T, T] {
  override val name: String = "sum"
}

object Sum {
  case object IntSum extends Sum[Int]
  case object DoubleSum extends Sum[Double]

  def apply(field: IntField): Field[Int] =
    Field(field.field, new Sum[Int] {})(field.field.adapter)

  def apply(field: DoubleField): Field[Double] =
    Field(field.field, new Sum[Double] {})(field.field.adapter)
}

final case class Count[T]() extends Grouping[T, Int] {
  override val name: String = "count"
}

object Count {
  implicit def apply[T](implicit fieldAdapter: FieldAdapter[T]): Count[T] =
    Count[T]()

  def apply[T](field: Field[T])(implicit fieldAdapter: FieldAdapter[Int]): Aggregation[T, Int] =
    Field(field, new Count[T])
}

final case class Max[T]() extends Grouping[T, Int] {
  override val name: String = "max"
}

object Max {
  implicit def apply[T](implicit fieldAdapter: FieldAdapter[T]): Max[T] =
    Max[T]()

  def apply[T](field: Field[T])(implicit fieldAdapter: FieldAdapter[Int]): Aggregation[T, Int] =
    Field(field, new Max[T])
}

final case class Min[T]() extends Grouping[T, Int] {
  override val name: String = "min"
}

object Min {
  implicit def apply[T](implicit fieldAdapter: FieldAdapter[T]): Min[T] =
    Min[T]()

  def apply[T](field: Field[T])(implicit fieldAdapter: FieldAdapter[Int]): Aggregation[T, Int] =
    Field(field, new Min[T])
}
