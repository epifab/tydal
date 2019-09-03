package io.epifab.yadl.typesafe

object Implicits {
  implicit class ExtendedGenericTerm[T](field: Field[T]) {
    def ===[U](field2: Field[U])(implicit comparable: Comparable[T, U]): BinaryExpr =
      Equals(field, field2)

    def !==[U](field2: Field[U])(implicit comparable: Comparable[T, U]): BinaryExpr =
      NotEquals(field, field2)

    def <[U](field2: Field[U])(implicit comparable: Comparable[T, U]): BinaryExpr =
      LessThan(field, field2)

    def >[U](field2: Field[U])(implicit comparable: Comparable[T, U]): BinaryExpr =
      GreaterThan(field, field2)

    def <=[U](field2: Field[U])(implicit comparable: Comparable[T, U]): BinaryExpr =
      LessThanOrEqual(field, field2)

    def >=[U](field2: Field[U])(implicit comparable: Comparable[T, U]): BinaryExpr =
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
