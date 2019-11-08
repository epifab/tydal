package io.epifab.tydal.fields

import io.epifab.tydal.fields

abstract class DbFunction[+T](val name: String)

abstract class DbFunction1[+F, +T](name: String) extends DbFunction[T](name)
abstract class DbFunction2[+F1, +F2, +T](name: String) extends DbFunction[T](name)
abstract class DbAggregationFunction[+F, +T](name: String) extends DbFunction[T](name)

object Avg {
  final private class Avg[+F <: Field[_]] extends DbAggregationFunction[F, Option[Double]]("avg")

  def apply[F <: Field[_]](field: F)(implicit isNumeric: IsNumeric[F], decoder: FieldDecoder[Option[Double]]): Aggregation[F, Option[Double]] =
    fields.Aggregation(field, new Avg[F])(decoder)
}

object Sum {
  final private class Sum[+F <: Field[_], +U] extends DbAggregationFunction[F, Option[U]]("sum")

  def apply[F <: Field[_]](field: F)(implicit isInteger: IsInteger[F], decoder: FieldDecoder[Option[Int]]): Aggregation[F, Option[Int]] =
    fields.Aggregation(field, new Sum[F, Int])(decoder)

  def apply[F <: Field[_]](field: F)(implicit isDouble: IsDouble[F], decoder: FieldDecoder[Option[Double]]): Aggregation[F, Option[Double]] =
    fields.Aggregation(field, new Sum[F, Double])(decoder)
}

object Count {
  final private class Count[+F <: Field[_]]() extends DbAggregationFunction[F, Int]("count")

  def apply[F <: Field[_]](field: F)(implicit decoder: FieldDecoder[Int]): Aggregation[F, Int] =
    fields.Aggregation(field, new Count[F])
}

object Max {
  final private class Max[+F <: Field[_], T] extends DbAggregationFunction[F, Option[T]]("max")

  def apply[F <: Field[_], T](field: F)(implicit fieldT: FieldT[F, T], fieldDecoder: FieldDecoder[Option[T]]): Aggregation[F, Option[T]] =
    fields.Aggregation(field, new Max[F, T])
}

object Min {
  final private class Min[+F <: Field[_], +T] extends DbAggregationFunction[F, Option[T]]("min")

  def apply[F <: Field[_], T](field: F)(implicit fieldT: FieldT[F, T], fieldDecoder: FieldDecoder[Option[T]]): Aggregation[F, Option[T]] =
    fields.Aggregation(field, new Min[F, T])
}
