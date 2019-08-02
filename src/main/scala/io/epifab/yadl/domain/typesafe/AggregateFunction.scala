package io.epifab.yadl.domain.typesafe

import io.epifab.yadl.domain.typesafe.fields.FieldDecoder

sealed trait AggregateFunction[+T, +U] {
  def name: String
}

object Avg {
  final private class Avg[+T] extends AggregateFunction[T, Option[Double]] {
    override val name: String = "avg"
  }

  def apply[T](term: Term[T])(implicit isNumeric: IsNumeric[T], decoder: FieldDecoder[Option[Double]]): Aggregation[T, Option[Double]] =
    Aggregation(term, new Avg[T])(decoder)
}

object Sum {
  final private class Sum[+T, +U] extends AggregateFunction[T, Option[U]] {
    override val name: String = "sum"
  }

  def apply[T](term: Term[T])(implicit isInteger: IsInteger[T], decoder: FieldDecoder[Option[Int]]): Aggregation[T, Option[Int]] =
    Aggregation(term, new Sum[T, Int])(decoder)

  def apply[T](term: Term[T])(implicit isDouble: IsDouble[T], decoder: FieldDecoder[Option[Double]]): Aggregation[T, Option[Double]] =
    Aggregation(term, new Sum[T, Double])(decoder)
}

object Count {
  final private class Count[+T]() extends AggregateFunction[T, Int] {
    override val name: String = "count"
  }

  def apply[T](term: Term[T])(implicit decoder: FieldDecoder[Int]): Aggregation[T, Int] =
    Aggregation(term, new Count[T])
}

object Max {
  final private class Max[+T] extends AggregateFunction[T, Option[T]] {
    override val name: String = "max"
  }

  def apply[T](term: Term[T])(implicit decoder: FieldDecoder[Option[T]]): Aggregation[T, Option[T]] =
    Term(term, new Max[T])
}

object Min {
  final private class Min[+T] extends AggregateFunction[T, Option[T]] {
    override val name: String = "min"
  }

  def apply[T](term: Term[T])(implicit decoder: FieldDecoder[Option[T]]): Aggregation[T, Option[T]] =
    Term(term, new Min[T])
}
