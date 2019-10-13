package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields._

object Implicits {
  implicit class ExtendedField[F1 <: Field[_]](field1: F1) {
    def ===[F2 <: Field[_]](field2: F2)(implicit comparable: fields.Comparable[F1, F2]): Equals[F1, F2] =
      Equals(field1, field2)

    def !==[F2 <: Field[_]](field2: F2)(implicit comparable: fields.Comparable[F1, F2]): NotEquals[F1, F2] =
      NotEquals(field1, field2)

    def <[F2 <: Field[_]](field2: F2)(implicit comparable: fields.Comparable[F1, F2]): LessThan[F1, F2] =
      LessThan(field1, field2)

    def >[F2 <: Field[_]](field2: F2)(implicit comparable: fields.Comparable[F1, F2]): GreaterThan[F1, F2] =
      GreaterThan(field1, field2)

    def <=[F2 <: Field[_]](field2: F2)(implicit comparable: fields.Comparable[F1, F2]): LessThanOrEqual[F1, F2] =
      LessThanOrEqual(field1, field2)

    def >=[F2 <: Field[_]](field2: F2)(implicit comparable: fields.Comparable[F1, F2]): GreaterThanOrEqual[F1, F2] =
      GreaterThanOrEqual(field1, field2)

    def subsetOf[F2 <: Field[_]](field2: F2)(implicit canBeSubset: CanBeSubset[F1, F2]): IsSubset[F1, F2] =
      IsSubset(field1, field2)

    def supersetOf[F2 <: Field[_]](field2: F2)(implicit canBeSuperset: CanBeSuperset[F1, F2]): IsSuperset[F1, F2] =
      IsSuperset(field1, field2)

    def overlaps[F2 <: Field[_]](field2: F2)(implicit canOverlap: CanOverlap[F1, F2]): Overlaps[F1, F2] =
      Overlaps(field1, field2)

    def in[F2 <: Field[_]](field2: F2)(implicit canBeIncluded: CanBeIncluded[F1, F2]): IsIncluded[F1, F2] =
      IsIncluded(field1, field2)
  }
}
