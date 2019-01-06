package io.epifab.yadl.domain

import io.epifab.yadl.domain.Field._

import scala.language.implicitConversions

sealed trait AggregateFunction[T, U] {
  def name: String
}

object Avg {
  final private class Avg[T] extends AggregateFunction[T, Option[Double]] {
    override val name: String = "avg"
  }

  def apply(field: IntField)(implicit adapter: FieldAdapter[Option[Double]]): Aggregation[Int, Option[Double]] =
    Aggregation(field.value, new Avg[Int])(adapter)

  def apply(field: DoubleField)(implicit adapter: FieldAdapter[Option[Double]]): Aggregation[Double, Option[Double]] =
    Aggregation(field.value, new Avg[Double])(adapter)
}

object Sum {
  final private class Sum[T] extends AggregateFunction[T, Option[T]] {
    override val name: String = "sum"
  }

  def apply(field: IntField)(implicit adapter: FieldAdapter[Option[Int]]): Aggregation[Int, Option[Int]] =
    Field(field.value, new Sum[Int])

  def apply(field: DoubleField)(implicit adapter: FieldAdapter[Option[Double]]): Aggregation[Double, Option[Double]] =
    Field(field.value, new Sum[Double])
}

object Count {
  final private class Count[T]() extends AggregateFunction[T, Int] {
    override val name: String = "count"
  }

  def apply[T](field: Field[T])(implicit adapter: FieldAdapter[Int]): Aggregation[T, Int] =
    Field(field, new Count[T])
}

object Max {
  final private class Max[T] extends AggregateFunction[T, Option[T]] {
    override val name: String = "max"
  }

  def apply[T](field: Field[T])(implicit adapter: FieldAdapter[Option[T]]): Aggregation[T, Option[T]] =
    Field(field, new Max[T])
}

object Min {
  final private class Min[T] extends AggregateFunction[T, Option[T]] {
    override val name: String = "min"
  }

  def apply[T](field: Field[T])(implicit adapter: FieldAdapter[Option[T]]): Aggregation[T, Option[T]] =
    Field(field, new Min[T])
}
