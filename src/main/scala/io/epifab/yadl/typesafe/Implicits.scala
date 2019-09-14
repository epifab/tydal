package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields.{BinaryExpr, CanBeIncluded, CanBeSubset, CanBeSuperset, CanOverlap, Equals, Field, GreaterThan, GreaterThanOrEqual, IsIncluded, IsSubset, IsSuperset, LessThan, LessThanOrEqual, NotEquals, Overlaps}

object Implicits {
  implicit class ExtendedField[T](field: Field[T]) {
    def ===[U](field2: Field[U])(implicit comparable: fields.Comparable[T, U]): BinaryExpr =
      Equals(field, field2)

    def !==[U](field2: Field[U])(implicit comparable: fields.Comparable[T, U]): BinaryExpr =
      NotEquals(field, field2)

    def <[U](field2: Field[U])(implicit comparable: fields.Comparable[T, U]): BinaryExpr =
      LessThan(field, field2)

    def >[U](field2: Field[U])(implicit comparable: fields.Comparable[T, U]): BinaryExpr =
      GreaterThan(field, field2)

    def <=[U](field2: Field[U])(implicit comparable: fields.Comparable[T, U]): BinaryExpr =
      LessThanOrEqual(field, field2)

    def >=[U](field2: Field[U])(implicit comparable: fields.Comparable[T, U]): BinaryExpr =
      GreaterThanOrEqual(field, field2)

    def subsetOf[U](field2: Field[U])(implicit canBeSubset: CanBeSubset[T, U]): BinaryExpr =
      IsSubset(field, field2)

    def supersetOf[U](field2: Field[U])(implicit canBeSuperset: CanBeSuperset[T, U]): BinaryExpr =
      IsSuperset(field, field2)

    def overlaps[U](field2: Field[U])(implicit canOverlap: CanOverlap[T, U]): BinaryExpr =
      Overlaps(field, field2)

    def in[U](field2: Field[U])(implicit canBeIncluded: CanBeIncluded[T, U]): BinaryExpr =
      IsIncluded(field, field2)
  }
}
