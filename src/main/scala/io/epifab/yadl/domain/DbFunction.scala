package io.epifab.yadl.domain

sealed abstract class DbFunction(val name: String)

sealed abstract class DbFunction1[T, U](name: String) extends DbFunction(name)
sealed abstract class DbFunction2[T1, T2, U](name: String) extends DbFunction(name)

sealed abstract class Latitude[T, U] extends DbFunction1[T, U]("ST_X")
sealed abstract class Longitude[T, U] extends DbFunction1[T, U]("ST_Y")

object Latitude {
  implicit object pure extends Latitude[Geometry, Double]
  implicit object optional extends Latitude[Option[Geometry], Option[Double]]

  def apply[T, U](term: Term[T])(implicit function: Latitude[T, U], adapter: FieldAdapter[U]) =
    Function1(term, function)
}

object Longitude {
  implicit object pure extends Longitude[Geometry, Double]
  implicit object optional extends Longitude[Option[Geometry], Option[Double]]

  def apply[T, U](term: Term[T])(implicit function: Longitude[T, U], adapter: FieldAdapter[U]) =
    Function1(term, function)
}

sealed abstract class Distance[T1, T2, U] extends DbFunction2[T1, T2, U]("ST_Distance")
sealed abstract class MakePoint[T1, T2, U] extends DbFunction2[T1, T2, U]("ST_MakePoint")

object Distance {
  implicit object geometryPure extends Distance[Geometry, Geometry, Double]
  implicit object geometryPptional1 extends Distance[Option[Geometry], Geometry, Option[Double]]
  implicit object geometryPptional2 extends Distance[Geometry, Option[Geometry], Option[Double]]
  implicit object geometryPptional3 extends Distance[Option[Geometry], Option[Geometry], Option[Double]]

  implicit object geographyPure extends Distance[Geography, Geography, Double]
  implicit object geographyPptional1 extends Distance[Option[Geography], Geography, Option[Double]]
  implicit object geographyPptional2 extends Distance[Geography, Option[Geography], Option[Double]]
  implicit object geographyPptional3 extends Distance[Option[Geography], Option[Geography], Option[Double]]

  def apply[T1, T2, U](term1: Term[T1], term2: Term[T2])(implicit function: Distance[T1, T2, U], adapter: FieldAdapter[U]) =
    Function2(term1, term2, function)
}

object MakePoint {
  implicit object pure extends MakePoint[Double, Double, Geometry]
  implicit object optional1 extends MakePoint[Option[Double], Double, Option[Geometry]]
  implicit object optional2 extends MakePoint[Double, Option[Double], Option[Geometry]]
  implicit object optional3 extends MakePoint[Option[Double], Option[Double], Option[Geometry]]

  def apply[T1, T2, U](term1: Term[T1], term2: Term[T2])(implicit function: MakePoint[T1, T2, U], adapter: FieldAdapter[U]) =
    Function2(term1, term2, function)
}
