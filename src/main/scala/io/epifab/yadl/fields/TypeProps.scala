package io.epifab.yadl.fields

import java.time.{Instant, LocalDate}

sealed trait TypeProps

trait IsNumeric[T] extends TypeProps
trait IsInteger[T] extends TypeProps
trait IsDouble[T] extends TypeProps
trait IsText[T] extends TypeProps
trait IsDate[T] extends TypeProps
trait IsDateTime[T] extends TypeProps

object IsNumeric {
  implicit def int[T](implicit isInteger: IsInteger[T]): IsNumeric[T] = new IsNumeric[T] {}
  implicit def double[T](implicit isDouble: IsDouble[T]): IsNumeric[T] = new IsNumeric[T] {}
}

object IsInteger {
  implicit val int: IsInteger[Int] = new IsInteger[Int] {}
  implicit def optional[T](implicit isInteger: IsInteger[T]): IsInteger[Option[T]] = new IsInteger[Option[T]] {}

  implicit def field[F <: Field[_], T](implicit ft: FieldT[F, T], isInteger: IsInteger[T]): IsInteger[F] = new IsInteger[F] {}
}

object IsDouble {
  implicit val double: IsDouble[Double] = new IsDouble[Double] {}
  implicit def optional[T](implicit isDouble: IsDouble[T]): IsDouble[Option[T]] = new IsDouble[Option[T]] {}

  implicit def field[F <: Field[_], T](implicit ft: FieldT[F, T], isDouble: IsDouble[T]): IsDouble[F] = new IsDouble[F] {}
}

object IsText {
  implicit val string: IsText[String] = new IsText[String] {}
  implicit def optional[T](implicit isText: IsText[T]): IsText[Option[T]] = new IsText[Option[T]] {}

  implicit def field[F <: Field[_], T](implicit ft: FieldT[F, T], isText: IsText[T]): IsText[F] = new IsText[F] {}
}

object IsDate {
  implicit val localDate: IsDate[LocalDate] = new IsDate[LocalDate] {}
  implicit def optional[T](implicit isDate: IsDate[T]): IsDate[Option[T]] = new IsDate[Option[T]] {}

  implicit def field[F <: Field[_], T](implicit ft: FieldT[F, T], isDate: IsDate[T]): IsDate[F] = new IsDate[F] {}
}

object IsDateTime {
  implicit val instant: IsDateTime[Instant] = new IsDateTime[Instant] {}
  implicit def optional[T](implicit isDateTime: IsDateTime[T]): IsDateTime[Option[T]] = new IsDateTime[Option[T]] {}

  implicit def field[F <: Field[_], T](implicit ft: FieldT[F, T], isDateTime: IsDateTime[T]): IsDateTime[F] = new IsDateTime[F] {}
}

sealed trait AreComparable[T, U] extends TypeProps

object AreComparable {
  implicit def numeric[T, U](implicit tIsNumeric: IsNumeric[T], uIsNumeric: IsNumeric[U]): AreComparable[T, U] =
    new AreComparable[T, U] {}

  implicit def text[T, U](implicit tIsText: IsText[T], uIsText: IsText[U]): AreComparable[T, U] =
    new AreComparable[T, U] {}

  implicit def date[T, U](implicit tIsDate: IsDate[T], uIsDate: IsDate[U]): AreComparable[T, U] =
    new AreComparable[T, U] {}

  implicit def dateTime[T, U](implicit tIsDateTime: IsDateTime[T], uIsDateTime: IsDateTime[U]): AreComparable[T, U] =
    new AreComparable[T, U] {}
}

sealed trait CanBeSuperset[T, U] extends TypeProps
sealed trait CanBeSubset[T, U] extends TypeProps
sealed trait CanOverlap[T, U] extends TypeProps
sealed trait CanBeIncluded[T, U] extends TypeProps

object CanBeSuperset {
  implicit def pure[T]: CanBeSuperset[Seq[T], Seq[T]] = new CanBeSuperset[Seq[T], Seq[T]] {}

  implicit def field[F <: Field[_], G <: Field[_], T, U]
      (implicit
       ft: FieldT[F, T],
       gt: FieldT[G, U],
       canBeSuperset: CanBeSuperset[T, U]): CanBeSuperset[F, G] =
    new CanBeSuperset[F, G] {}
}

object CanBeSubset {
  implicit def pure[T]: CanBeSubset[Seq[T], Seq[T]] = new CanBeSubset[Seq[T], Seq[T]] {}

  implicit def field[F <: Field[_], G <: Field[_], T, U]
      (implicit
       ft: FieldT[F, T],
       gt: FieldT[G, U],
       canBeSubset: CanBeSubset[T, U]): CanBeSubset[F, G] =
    new CanBeSubset[F, G] {}
}

object CanOverlap {
  implicit def pure[T]: CanOverlap[Seq[T], Seq[T]] = new CanOverlap[Seq[T], Seq[T]] {}
  implicit def optionLeft[T]: CanOverlap[Option[Seq[T]], Seq[T]] = new CanOverlap[Option[Seq[T]], Seq[T]] {}
  implicit def optionRight[T]: CanOverlap[Seq[T], Option[Seq[T]]] = new CanOverlap[Seq[T], Option[Seq[T]]] {}
  implicit def option[T]: CanOverlap[Option[Seq[T]], Option[Seq[T]]] = new CanOverlap[Option[Seq[T]], Option[Seq[T]]] {}

  implicit def field[F <: Field[_], G <: Field[_], T, U]
      (implicit
       ft: FieldT[F, T],
       gt: FieldT[G, U],
       canOverlap: CanOverlap[T, U]): CanOverlap[F, G] =
    new CanOverlap[F, G] {}
}

object CanBeIncluded {
  implicit def pure[T]: CanBeIncluded[T, Seq[T]] = new CanBeIncluded[T, Seq[T]] {}
  implicit def optionLeft[T]: CanBeIncluded[Option[T], Seq[T]] = new CanBeIncluded[Option[T], Seq[T]] {}
  implicit def optionRight[T]: CanBeIncluded[T, Option[Seq[T]]] = new CanBeIncluded[T, Option[Seq[T]]] {}
  implicit def option[T]: CanBeIncluded[Option[T], Option[Seq[T]]] = new CanBeIncluded[Option[T], Option[Seq[T]]] {}

  implicit def field[F <: Field[_], G <: Field[_], T, U]
      (implicit
       ft: FieldT[F, T],
       gt: FieldT[G, U],
       canOverlap: CanBeIncluded[T, U]): CanBeIncluded[F, G] =
    new CanBeIncluded[F, G] {}
}
