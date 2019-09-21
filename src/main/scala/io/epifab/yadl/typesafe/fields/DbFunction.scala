package io.epifab.yadl.typesafe.fields

import io.epifab.yadl.typesafe.fields

abstract class DbFunction[+U](val name: String)

abstract class DbFunction1[+T, +U](name: String) extends DbFunction[U](name)
abstract class DbFunction2[+T1, +T2, +U](name: String) extends DbFunction[U](name)
abstract class DbFunction3[+T1, +T2, +T3, +U](name: String) extends DbFunction[U](name)
abstract class DbAggregationFunction[+T, +U](name: String) extends DbFunction[U](name)

object Avg {
  final private class Avg[+T] extends DbAggregationFunction[T, Option[Double]]("avg")

  def apply[T](field: Field[T])(implicit isNumeric: IsNumeric[T], decoder: FieldDecoder[Option[Double]]): Aggregation[T, Option[Double]] =
    fields.Aggregation(field, new Avg[T])(decoder)
}

object Sum {
  final private class Sum[+T, +U] extends DbAggregationFunction[T, Option[U]]("sum")

  def apply[T](field: Field[T])(implicit isInteger: IsInteger[T], decoder: FieldDecoder[Option[Int]]): Aggregation[T, Option[Int]] =
    fields.Aggregation(field, new Sum[T, Int])(decoder)

  def apply[T](field: Field[T])(implicit isDouble: IsDouble[T], decoder: FieldDecoder[Option[Double]]): Aggregation[T, Option[Double]] =
    fields.Aggregation(field, new Sum[T, Double])(decoder)
}

object Count {
  final private class Count[+T]() extends DbAggregationFunction[T, Int]("count")

  def apply[T](field: Field[T])(implicit decoder: FieldDecoder[Int]): Aggregation[T, Int] =
    fields.Aggregation(field, new Count[T])
}

object Max {
  final private class Max[+T] extends DbAggregationFunction[T, Option[T]]("max")

  def apply[T](field: Field[T])(implicit decoder: FieldDecoder[Option[T]]): Aggregation[T, Option[T]] =
    fields.Aggregation(field, new Max[T])
}

object Min {
  final private class Min[+T] extends DbAggregationFunction[T, Option[T]]("min")

  def apply[T](field: Field[T])(implicit decoder: FieldDecoder[Option[T]]): Aggregation[T, Option[T]] =
    fields.Aggregation(field, new Min[T])
}
