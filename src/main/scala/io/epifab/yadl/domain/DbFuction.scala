package io.epifab.yadl.domain

import io.epifab.yadl.domain.Column._

import scala.language.implicitConversions

sealed trait DbFuction[T, U]

sealed trait AggregateFunction[T, U] extends DbFuction[T, U] {
  def name: String
}

final class Avg[T] extends AggregateFunction[T, Option[Double]] {
  override val name: String = "avg"
}

object Avg {
  def apply(column: IntColumn)(implicit adapter: FieldAdapter[Option[Double]]): AggregateColumn[Int, Option[Double]] =
    AggregateColumn(column.value, new Avg[Int])(adapter)

  def apply(column: DoubleColumn)(implicit adapter: FieldAdapter[Option[Double]]): AggregateColumn[Double, Option[Double]] =
    AggregateColumn(column.value, new Avg[Double])(adapter)
}

final class Sum[T] extends AggregateFunction[T, T] {
  override val name: String = "sum"
}

object Sum {
  def apply(column: IntColumn): Column[Int] =
    Column(column.value, new Sum[Int])(column.value.adapter)

  def apply(column: DoubleColumn): Column[Double] =
    Column(column.value, new Sum[Double])(column.value.adapter)
}

final case class Count[T]() extends AggregateFunction[T, Int] {
  override val name: String = "count"
}

object Count {
  def apply[T](column: Column[T])(implicit fieldAdapter: FieldAdapter[Int]): AggregateColumn[T, Int] =
    Column(column, new Count[T])
}

final class Max[T] extends AggregateFunction[T, Int] {
  override val name: String = "max"
}

object Max {
  def apply[T](column: Column[T])(implicit fieldAdapter: FieldAdapter[Int]): AggregateColumn[T, Int] =
    Column(column, new Max[T])
}

final class Min[T] extends AggregateFunction[T, Int] {
  override val name: String = "min"
}

object Min {
  def apply[T](column: Column[T])(implicit fieldAdapter: FieldAdapter[Int]): AggregateColumn[T, Int] =
    Column(column, new Min[T])
}
