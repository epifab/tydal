package io.epifab.tydal.fields

import java.time.{Instant, LocalDate}

import scala.annotation.implicitAmbiguous

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

sealed trait AreComparableSeq[T, U] extends TypeProps
sealed trait CanBeIncluded[T, U] extends TypeProps

object AreComparableSeq {
  implicit def pure[T]: AreComparableSeq[Seq[T], Seq[T]] = new AreComparableSeq[Seq[T], Seq[T]] {}
  implicit def optionLeft[T]: AreComparableSeq[Option[Seq[T]], Seq[T]] = new AreComparableSeq[Option[Seq[T]], Seq[T]] {}
  implicit def optionRight[T]: AreComparableSeq[Seq[T], Option[Seq[T]]] = new AreComparableSeq[Seq[T], Option[Seq[T]]] {}
  implicit def option[T]: AreComparableSeq[Option[Seq[T]], Option[Seq[T]]] = new AreComparableSeq[Option[Seq[T]], Option[Seq[T]]] {}

  implicit def field[F <: Field[_], G <: Field[_], T, U]
      (implicit
       ft: FieldT[F, T],
       gt: FieldT[G, U],
       canOverlap: AreComparableSeq[T, U]): AreComparableSeq[F, G] =
    new AreComparableSeq[F, G] {}
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

sealed trait IsOptional[F] extends TypeProps

object IsOptional {
  implicit def optional[T]: IsOptional[Option[T]] = new IsOptional[Option[T]] {}

  implicit def field[F <: Field[_], T]
      (implicit
       ft: FieldT[F, T],
       isOptional: IsOptional[T]): IsOptional[F] =
    new IsOptional[F] {}
}

sealed trait Negative[A]

object Negative {
  implicit def negative[T](implicit t: T): Negative[T] = new Negative[T] {}
  @implicitAmbiguous("Cannot find a negative type class ${A}")
  implicit def positive[T]: Negative[T] = new Negative[T] {}
}

sealed trait NullableField[F <: Field[_], G <: Field[_]] {
  def apply(f: F): G
}

object NullableField {
  implicit def nonOptional[F <: Field[_], T]
      (implicit
       fieldT: FieldT[F, T],
       nonOptional: Negative[IsOptional[F]],
       decoder: FieldDecoder[Option[T]]): NullableField[F, SoftCast[F, Option[T]]] =
    new NullableField[F, SoftCast[F, Option[T]]] {
      override def apply(f: F): SoftCast[F, Option[T]] = SoftCast[F, Option[T]](f)
    }

  implicit def optional[F <: Field[_], T]
      (implicit
       fieldT: FieldT[F, T],
       optional: IsOptional[F]): NullableField[F, F] = new NullableField[F, F] {
    override def apply(f: F): F = f
  }
}
