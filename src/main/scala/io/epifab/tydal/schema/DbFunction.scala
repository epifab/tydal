package io.epifab.tydal.schema

abstract class DbFunction[+T](val name: String)

abstract class DbFunction1[+F, +T](name: String) extends DbFunction[T](name)
abstract class DbFunction2[+F1, +F2, +T](name: String) extends DbFunction[T](name)
abstract class DbAggregationFunction[+F, +T](name: String) extends DbFunction[T](name)

object Distinct {
  class Distinct1[+F <: Field[_], T] extends DbFunction1[F, T]("distinct")

  def apply[F <: Field[_], T](field: F)(implicit fieldT: FieldT[F, T]): FieldExpr1[F, T] =
    FieldExpr1(field, new Distinct1[F, T])(fieldT.get(field).decoder)
}

object Avg {
  final private class Avg[+F <: Field[_]] extends DbAggregationFunction[F, Option[Double]]("avg")

  def apply[F <: Field[_]](field: F)(implicit isNumeric: IsNumeric[F], decoder: FieldDecoder[Option[Double]]): Aggregation[F, Option[Double]] =
    Aggregation(field, new Avg[F])(decoder)
}

object Sum {
  final private class Sum[+F <: Field[_], +U] extends DbAggregationFunction[F, Option[U]]("sum")

  def apply[F <: Field[_]](field: F)(implicit isInteger: IsInteger[F], decoder: FieldDecoder[Option[Int]]): Aggregation[F, Option[Int]] =
    Aggregation(field, new Sum[F, Int])(decoder)

  def apply[F <: Field[_]](field: F)(implicit isDouble: IsDouble[F], decoder: FieldDecoder[Option[Double]]): Aggregation[F, Option[Double]] =
    Aggregation(field, new Sum[F, Double])(decoder)
}

object Count {
  final private class Count[+F <: Field[_]]() extends DbAggregationFunction[F, Long]("count")

  def apply[F <: Field[_]](field: F)(implicit decoder: FieldDecoder[Long]): Aggregation[F, Long] =
    Aggregation(field, new Count[F])
}

object Max {
  final private class Max[+F <: Field[_], T] extends DbAggregationFunction[F, T]("max")

  def apply[F <: Field[_], G <: Field[_], T](field: F)(implicit nullableF: NullableField[F, G], fieldT: FieldT[G, T], decoder: FieldDecoder[T]): Aggregation[F, T] =
    Aggregation(field, new Max[F, T])
}

object Min {
  final private class Min[+F <: Field[_], +T] extends DbAggregationFunction[F, T]("min")

  def apply[F <: Field[_], G <: Field[_], T](field: F)(implicit nullableF: NullableField[F, G], fieldT: FieldT[G, T], decoder: FieldDecoder[T]): Aggregation[F, T] =
    Aggregation(field, new Min[F, T])
}
