package io.epifab.tydal.typesafe

import java.time.{Instant, LocalDate}

import io.epifab.tydal.Tag
import org.scalatest.FunSuite
import shapeless.the

import scala.annotation.implicitAmbiguous

trait SameType[T, U]

object SameType {
  implicit def trivial[T]: SameType[T, T] = new SameType[T, T] {}
}

class TypePropsTest extends FunSuite {
  import io.epifab.tydal.fields._

  test("Negative type class") {
    the[SameType[Int, Int]]
    the[Negative[SameType[Int, String]]]
    the[Negative[Negative[SameType[Int, Int]]]]
  }

  test("IsNumeric") {
    the[IsNumeric[Int]]
    the[IsNumeric[Field[Option[Int]]]]
    the[IsNumeric[Field[Int]]]
    the[IsNumeric[Column[Int] with Tag["test"]]]
    the[IsNumeric[Column[Int]]]

    the[IsNumeric[Double]]
    the[IsNumeric[Option[Double]]]
    the[IsNumeric[Field[Double]]]
    the[IsNumeric[Column[Double] with Tag["test"]]]
    the[IsNumeric[Column[Double]]]

    the[Negative[IsNumeric[String]]]
    the[Negative[IsNumeric[Instant]]]
    the[Negative[IsNumeric[Seq[Int]]]]
  }

  test("IsText") {
    // the[IsText[String]]
    the[IsText[Field[Option[String]]]]
    the[IsText[Field[String]]]
    the[IsText[Column[String] with Tag["test"]]]
    the[IsText[Column[String]]]

    the[Negative[IsText[Int]]]
    the[Negative[IsText[Instant]]]
    the[Negative[IsText[LocalDate]]]
    the[Negative[IsText[Seq[String]]]]
  }

  test("IsDate") {
    // the[IsTemporal[Instant]];
    the[IsDate[Field[Option[LocalDate]]]]
    the[IsDate[Field[LocalDate]]]
    the[IsDate[Column[LocalDate] with Tag["test"]]]
    the[IsDate[Column[LocalDate]]]

    the[Negative[IsDate[Int]]]
    the[Negative[IsDate[String]]]
    the[Negative[IsDate[Instant]]]
    the[Negative[IsDate[Seq[LocalDate]]]]
  }

  test("IsDateTime") {
    // the[IsTemporal[Instant]];
    the[IsDateTime[Field[Option[Instant]]]]
    the[IsDateTime[Field[Instant]]]
    the[IsDateTime[Column[Instant] with Tag["test"]]]
    the[IsDateTime[Column[Instant]]]

    the[Negative[IsDateTime[Int]]]
    the[Negative[IsDateTime[String]]]
    the[Negative[IsDateTime[LocalDate]]]
    the[Negative[IsDateTime[Seq[Instant]]]]
  }

  test("AreComparable") {
    // T with T and optional
    the[AreComparable[Int, Int]]
    the[AreComparable[Option[Int], Int]]
    the[AreComparable[Int, Option[Int]]]

    // Integers and doubles
    the[AreComparable[Double, Int]]
    the[AreComparable[Double, Option[Int]]]
    the[AreComparable[Double, Field[Option[Int]]]]

    // Text and time
    the[AreComparable[Column[String] with Tag["test"], NamedPlaceholder[String] with Tag["roar"]]]
    the[AreComparable[Column[Instant] with Tag["test"], NamedPlaceholder[Instant] with Tag["roar"]]]
    the[AreComparable[Column[LocalDate] with Tag["test"], NamedPlaceholder[LocalDate] with Tag["roar"]]]

    // Fields
    the[AreComparable[Int, Field[Int]]]
    the[AreComparable[Int, Field[Double]]]
    the[AreComparable[Int, Field[Option[Double]]]]
    the[AreComparable[Int, Column[Option[Double]]]]

    // Incomparable types
    the[Negative[AreComparable[Int, String]]]
    the[Negative[AreComparable[Field[Int], Field[String]]]]
  }

  test("AreComparableSeq") {
    the[AreComparableSeq[Seq[Int], Seq[Int]]]
    the[AreComparableSeq[Column[Seq[Int]], Field[Seq[Int]]]]

    the[Negative[AreComparableSeq[Int, Seq[Int]]]]
    the[Negative[AreComparableSeq[Seq[Seq[Int]], Seq[Int]]]]
  }

  test("CanBeIncluded") {
    the[CanBeIncluded[Int, Seq[Int]]]
    the[CanBeIncluded[Option[Int], Seq[Int]]]
    the[CanBeIncluded[Int, Option[Seq[Int]]]]
    the[CanBeIncluded[Option[Int], Option[Seq[Int]]]]

    the[CanBeIncluded[Column[Int], Field[Seq[Int]]]]
    the[CanBeIncluded[Column[Option[Int]], Field[Seq[Int]]]]
    the[CanBeIncluded[Column[Int], Field[Option[Seq[Int]]]]]
    the[CanBeIncluded[Column[Option[Int]], Field[Option[Seq[Int]]]]]

    the[Negative[CanBeIncluded[Seq[Int], Seq[Int]]]]
  }
}
