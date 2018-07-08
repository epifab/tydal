package io.epifab.yadl.domain

import scala.language.implicitConversions

sealed trait DbFuction[T, U]

sealed trait Grouping[T, U] extends DbFuction[T, U] {
  def name: String
}

sealed trait Avg[T] extends Grouping[T, Double] {
  override val name: String = "avg"
}

object Avg {
  case object IntAvg extends Avg[Int]
  case object DoubleAvg extends Avg[Double]

  def apply(field: Field[Int])(implicit adapter: FieldAdapter[Double]): Aggregation[Int, Double] =
    Field(field, IntAvg)(adapter)

  def apply(field: Field[Double]): Aggregation[Double, Double] =
    Field(field, DoubleAvg)(field.adapter)
}

sealed trait Sum[T] extends Grouping[T, T] {
  override val name: String = "sum"
}

object Sum {
  case object IntSum extends Sum[Int]
  case object DoubleSum extends Sum[Double]

  def ofInt(field: Field[Int]): Field[Int] =
    Field(field, IntSum)(field.adapter)

  def ofDouble(field: Field[Double]): Field[Double] =
    Field(field, DoubleSum)(field.adapter)
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
