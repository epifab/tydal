package io.epifab.yadl.domain

sealed abstract class DbFunction(val name: String)

sealed abstract class DbFunction1[T, U](name: String) extends DbFunction(name)
sealed abstract class DbFunction2[T1, T2, U](name: String) extends DbFunction(name)

sealed abstract class Latitude[T, U] extends DbFunction1[T, U]("ST_X")
sealed abstract class Longitude[T, U] extends DbFunction1[T, U]("ST_Y")

object Latitude {
  implicit object pure extends Latitude[Point, Double]
  implicit object optional extends Latitude[Option[Point], Option[Double]]

  def apply[T, U](term: Term[T])(implicit function: Latitude[T, U], adapter: FieldAdapter[U]) =
    Conversion1(term, function)
}

object Longitude {
  implicit object pure extends Longitude[Point, Double]
  implicit object optional extends Longitude[Option[Point], Option[Double]]

  def apply[T, U](term: Term[T])(implicit function: Longitude[T, U], adapter: FieldAdapter[U]) =
    Conversion1(term, function)
}

sealed abstract class Distance[T1, T2, U] extends DbFunction2[T1, T2, U]("ST_Distance")
sealed abstract class MakePoint[T1, T2, U] extends DbFunction2[T1, T2, U]("ST_MakePoint")

object Distance {
  implicit object pure extends Distance[Point, Point, Double]
  implicit object optional1 extends Distance[Option[Point], Point, Option[Double]]
  implicit object optional2 extends Distance[Point, Option[Point], Option[Double]]
  implicit object optional3 extends Distance[Option[Point], Option[Point], Option[Double]]

  def apply[T1, T2, U](term1: Term[T1], term2: Term[T2])(implicit function: Distance[T1, T2, U], adapter: FieldAdapter[U]) =
    Conversion2(term1, term2, function)
}

object MakePoint {
  implicit object pure extends MakePoint[Double, Double, Point]
  implicit object optional1 extends MakePoint[Option[Double], Double, Option[Point]]
  implicit object optional2 extends MakePoint[Double, Option[Double], Option[Point]]
  implicit object optional3 extends MakePoint[Option[Double], Option[Double], Option[Point]]

  def apply[T1, T2, U](term1: Term[T1], term2: Term[T2])(implicit function: MakePoint[T1, T2, U], adapter: FieldAdapter[U]) =
    Conversion2(term1, term2, function)
}
