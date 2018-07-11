package io.epifab.yadl.domain

import io.epifab.yadl.domain.Column._

import scala.language.implicitConversions

sealed trait AggregateFunction[T, U] {
  def name: String
}

object Avg {
  final private class Avg[T] extends AggregateFunction[T, Option[Double]] {
    override val name: String = "avg"
  }

  def apply(column: IntColumn)(implicit adapter: FieldAdapter[Option[Double]]): AggregateColumn[Int, Option[Double]] =
    AggregateColumn(column.value, new Avg[Int])(adapter)

  def apply(column: DoubleColumn)(implicit adapter: FieldAdapter[Option[Double]]): AggregateColumn[Double, Option[Double]] =
    AggregateColumn(column.value, new Avg[Double])(adapter)
}

object Sum {
  final private class Sum[T] extends AggregateFunction[T, Option[T]] {
    override val name: String = "sum"
  }

  def apply(column: IntColumn)(implicit adapter: FieldAdapter[Option[Int]]): AggregateColumn[Int, Option[Int]] =
    Column(column.value, new Sum[Int])

  def apply(column: DoubleColumn)(implicit adapter: FieldAdapter[Option[Double]]): AggregateColumn[Double, Option[Double]] =
    Column(column.value, new Sum[Double])
}

object Count {
  final private class Count[T]() extends AggregateFunction[T, Option[Int]] {
    override val name: String = "count"
  }

  def apply[T](column: Column[T])(implicit adapter: FieldAdapter[Option[Int]]): AggregateColumn[T, Option[Int]] =
    Column(column, new Count[T])
}

object Max {
  final private class Max[T] extends AggregateFunction[T, Option[T]] {
    override val name: String = "max"
  }

  def apply[T](column: Column[T])(implicit adapter: FieldAdapter[Option[T]]): AggregateColumn[T, Option[T]] =
    Column(column, new Max[T])
}

object Min {
  final private class Min[T] extends AggregateFunction[T, Option[T]] {
    override val name: String = "min"
  }

  def apply[T](column: Column[T])(implicit adapter: FieldAdapter[Option[T]]): AggregateColumn[T, Option[T]] =
    Column(column, new Min[T])
}
