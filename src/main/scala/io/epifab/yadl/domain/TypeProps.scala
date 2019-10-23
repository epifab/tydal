package io.epifab.yadl.domain

import java.time.{LocalDate, LocalDateTime}

import scala.annotation.implicitNotFound

sealed trait TypeProps

sealed trait IsNumeric[T] extends TypeProps
sealed trait IsInteger[T] extends TypeProps
sealed trait IsDouble[T] extends TypeProps
sealed trait IsText[T] extends TypeProps
sealed trait IsDate[T] extends TypeProps
sealed trait IsDateTime[T] extends TypeProps
sealed trait IsNullable[T] extends TypeProps
sealed trait IsNotNullable[T] extends TypeProps

object IsNumeric {
  implicit def int[T](implicit isInteger: IsInteger[T]): IsNumeric[T] = new IsNumeric[T] {}
  implicit def double[T](implicit isDouble: IsDouble[T]): IsNumeric[T] = new IsNumeric[T] {}
}

object IsInteger {
  implicit val pure: IsInteger[Int] = new IsInteger[Int] {}
  implicit def optional[T](implicit isNumeric: IsInteger[T]): IsInteger[Option[T]] = new IsInteger[Option[T]] {}
}

object IsDouble {
  implicit val pure: IsDouble[Double] = new IsDouble[Double] {}
  implicit def optional[T](implicit isNumeric: IsDouble[T]): IsDouble[Option[T]] = new IsDouble[Option[T]] {}
}

object IsText {
  implicit val pure: IsText[String] = new IsText[String] {}
  implicit def optional[T](implicit textLike: IsText[T]): IsText[Option[T]] = new IsText[Option[T]] {}
}

object IsDate {
  implicit val pure: IsDate[LocalDate] = new IsDate[LocalDate] {}
  implicit def optional[T](implicit textLike: IsDate[T]): IsDate[Option[T]] = new IsDate[Option[T]] {}
}

object IsDateTime {
  implicit val pure: IsDateTime[LocalDateTime] = new IsDateTime[LocalDateTime] {}
  implicit def optional[T](implicit textLike: IsDateTime[T]): IsDateTime[Option[T]] = new IsDateTime[Option[T]] {}
}

object IsNullable {
  implicit def pure[T]: IsNullable[Option[T]] = new IsNullable[Option[T]] {}
}

sealed trait Sortable[T] extends TypeProps

object Sortable {
  implicit def number[T](implicit isNumeric: IsNumeric[T]): Sortable[T] = new Sortable[T] {}
  implicit def text[T](implicit isText: IsText[T]): Sortable[T] = new Sortable[T] {}
  implicit def date[T](implicit isDate: IsDate[T]): Sortable[T] = new Sortable[T] {}
  implicit def dateTime[T](implicit isDateTime: IsDateTime[T]): Sortable[T] = new Sortable[T] {}
}

@implicitNotFound("${T} and ${U} are not comparable")
sealed trait Comparable[T, U] extends TypeProps

object Comparable {
  implicit def pure[T]: Comparable[T, T] = new Comparable[T, T] {}

  implicit def optionalRight[T, U](implicit comparable: Comparable[T, U]): Comparable[T, Option[U]] =
    new Comparable[T, Option[U]] {}

  implicit def optionalLeft[T, U](implicit comparable: Comparable[T, U]): Comparable[Option[T], U] =
    new Comparable[Option[T], U] {}

  implicit def numbers[T, U](implicit tIsNumeric: IsNumeric[T], uIsNumberic: IsNumeric[U]): Comparable[T, U] =
    new Comparable[T, U] {}
}

sealed trait CanBeSuperset[T, U] extends TypeProps
sealed trait CanBeSubset[T, U] extends TypeProps
sealed trait CanOverlap[T, U] extends TypeProps
sealed trait CanBeIncluded[T, U] extends TypeProps

object CanBeSuperset {
  implicit def pure[T]: CanBeSuperset[Seq[T], Seq[T]] = new CanBeSuperset[Seq[T], Seq[T]] {}
}

object CanBeSubset {
  implicit def pure[T]: CanBeSubset[Seq[T], Seq[T]] = new CanBeSubset[Seq[T], Seq[T]] {}
}

object CanOverlap {
  implicit def pure[T]: CanOverlap[Seq[T], Seq[T]] = new CanOverlap[Seq[T], Seq[T]] {}
}

object CanBeIncluded {
  implicit def pure[T]: CanBeIncluded[T, Seq[T]] = new CanBeIncluded[T, Seq[T]] {}
  implicit def optional[T]: CanBeIncluded[Option[T], Seq[T]] = new CanBeIncluded[Option[T], Seq[T]] {}
}
