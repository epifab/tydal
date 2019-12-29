package io.epifab.tydal.schema

import java.time.{Instant, LocalDate}
import java.util.UUID

import io.epifab.tydal.{Tagged, Tagging, Untagged}

import scala.annotation.{implicitAmbiguous, implicitNotFound}

trait TypeProps

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
  implicit val long: IsInteger[Long] = new IsInteger[Long] {}
  implicit def optional[T](implicit isInteger: IsInteger[T]): IsInteger[Option[T]] = new IsInteger[Option[T]] {}

  implicit def field[F <: Field[_], T](implicit ft: FieldT[F, T], isInteger: IsInteger[T]): IsInteger[F] = new IsInteger[F] {}
}

object IsDouble {
  implicit val double: IsDouble[Double] = new IsDouble[Double] {}
  implicit def optional[T](implicit isOptional: IsDouble[T]): IsDouble[Option[T]] = new IsDouble[Option[T]] {}

  implicit def field[F <: Field[_], T](implicit ft: FieldT[F, T], isDouble: IsDouble[T]): IsDouble[F] = new IsDouble[F] {}
}

object IsText {
  implicit val string: IsText[String] = new IsText[String] {}
  implicit val uuid: IsText[UUID] = new IsText[UUID] {}
  implicit def optional[T](implicit isOptional: IsText[T]): IsText[Option[T]] = new IsText[Option[T]] {}

  implicit def field[F <: Field[_], T](implicit ft: FieldT[F, T], isText: IsText[T]): IsText[F] = new IsText[F] {}
}

object IsDate {
  implicit val localDate: IsDate[LocalDate] = new IsDate[LocalDate] {}
  implicit def optional[T](implicit isOptional: IsDate[T]): IsDate[Option[T]] = new IsDate[Option[T]] {}

  implicit def field[F <: Field[_], T](implicit ft: FieldT[F, T], isDate: IsDate[T]): IsDate[F] = new IsDate[F] {}
}

object IsDateTime {
  implicit val instant: IsDateTime[Instant] = new IsDateTime[Instant] {}
  implicit def optional[T](implicit isOptional: IsDateTime[T]): IsDateTime[Option[T]] = new IsDateTime[Option[T]] {}

  implicit def field[F <: Field[_], T](implicit ft: FieldT[F, T], isDateTime: IsDateTime[T]): IsDateTime[F] = new IsDateTime[F] {}
}

@implicitNotFound("${T} and ${U} are not comparable")
sealed trait AreComparable[T, U] extends TypeProps

object AreComparable {
  def yes[A, B]: AreComparable[A, B] = new AreComparable[A, B] {}

  implicit def fields[F <: Field[_], G <: Field[_], A](
    implicit
    fType: FieldT[F, A],
    gType: FieldT[G, A]
  ): AreComparable[F, G] = yes

  implicit def optionF[F <: Field[_], G <: Field[_], A](
    implicit
    fType: FieldT[F, Option[A]],
    gType: FieldT[G, A]
  ): AreComparable[F, G] = yes

  implicit def optionG[F <: Field[_], G <: Field[_], A](
    implicit
    fType: FieldT[F, A],
    gType: FieldT[G, Option[A]]
  ): AreComparable[F, G] = yes

  implicit def numeric[T, U](implicit tIsNumeric: IsNumeric[T], uIsNumeric: IsNumeric[U]): AreComparable[T, U] =
    yes

  implicit def text[T, U](implicit tIsText: IsText[T], uIsText: IsText[U]): AreComparable[T, U] =
    yes

  implicit def date[T, U](implicit tIsDate: IsDate[T], uIsDate: IsDate[U]): AreComparable[T, U] =
    yes

  implicit def dateTime[T, U](implicit tIsDateTime: IsDateTime[T], uIsDateTime: IsDateTime[U]): AreComparable[T, U] =
    yes
}

@implicitNotFound("${T} and ${U} are not comparable")
sealed trait AreComparableSeq[T, U] extends TypeProps

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

@implicitNotFound("${T} cannot be included in ${U}")
sealed trait CanBeIncluded[T, U] extends TypeProps

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

sealed trait AnyOptional[F1, F2] extends TypeProps

object AnyOptional {
  implicit def first[F1, F2](implicit isOptional: IsOptional[F1], nonOptional: Negative[IsOptional[F2]]): AnyOptional[F1, F2] =
    new AnyOptional[F1, F2] {}

  implicit def second[F1, F2](implicit isOptional: IsOptional[F2], nonOptional: Negative[IsOptional[F1]]): AnyOptional[F1, F2] =
    new AnyOptional[F1, F2] {}

  implicit def both[F1, F2](implicit isOptional: IsOptional[F1], nonOptional: IsOptional[F2]): AnyOptional[F1, F2] =
    new AnyOptional[F1, F2] {}
}

sealed trait Negative[A]

object Negative {
  implicit def negative[T](implicit t: T): Negative[T] = new Negative[T] {}
  @implicitAmbiguous("Cannot negate ${T}")
  implicit def positive[T]: Negative[T] = new Negative[T] {}
}

sealed trait NullableField[F <: Field[_], G <: Field[_]] {
  def apply(f: F): G
}

object NullableField {
  implicit def nonOptionalUntagged[F <: Field[_], T]
      (implicit
       fieldT: FieldT[F, T],
       untagged: Untagged[F],
       nonOptional: Negative[IsOptional[F]],
       decoder: FieldDecoder[Option[T]]): NullableField[F, SoftCast[F, Option[T]]] =
    new NullableField[F, SoftCast[F, Option[T]]] {
      override def apply(f: F): SoftCast[F, Option[T]] = SoftCast[F, Option[T]](f)
    }

  implicit def nonOptionalTagged[F <: Field[_], T, A <: String with Singleton]
      (implicit
       fieldT: FieldT[F, T],
       tagged: Tagged[F, A],
       nonOptional: Negative[IsOptional[F]],
       decoder: FieldDecoder[Option[T]]): NullableField[F, SoftCast[F, Option[T]] with Tagging[A]] =
    new NullableField[F, SoftCast[F, Option[T]] with Tagging[A]] {
      override def apply(f: F): SoftCast[F, Option[T]] with Tagging[A] = SoftCast[F, Option[T]](f) as tagged.tag
    }

  implicit def optional[F <: Field[_], T]
      (implicit
       fieldT: FieldT[F, T],
       optional: IsOptional[F]): NullableField[F, F] = new NullableField[F, F] {
    override def apply(f: F): F = f
  }
}
