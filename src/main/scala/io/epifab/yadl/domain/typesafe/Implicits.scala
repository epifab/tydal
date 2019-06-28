package io.epifab.yadl.domain.typesafe

object Implicits {
  implicit class ExtendedGenericTerm[T](term: Term[T]) {
    def ===[U](term2: Term[U])(implicit comparable: Comparable[T, U]): BinaryExpr =
      Equals(term, term2)

    def !==[U](term2: Term[U])(implicit comparable: Comparable[T, U]): BinaryExpr =
      NotEquals(term, term2)

    def <[U](term2: Term[U])(implicit comparable: Comparable[T, U]): BinaryExpr =
      LessThan(term, term2)

    def >[U](term2: Term[U])(implicit comparable: Comparable[T, U]): BinaryExpr =
      GreaterThan(term, term2)

    def <=[U](term2: Term[U])(implicit comparable: Comparable[T, U]): BinaryExpr =
      LessThanOrEqual(term, term2)

    def >=[U](term2: Term[U])(implicit comparable: Comparable[T, U]): BinaryExpr =
      GreaterThanOrEqual(term, term2)

    def subsetOf[U](term2: Term[U])(implicit canBeSubset: CanBeSubset[T, U]): BinaryExpr =
      IsSubset(term, term2)

    def supersetOf[U](term2: Term[U])(implicit canBeSuperset: CanBeSuperset[T, U]): BinaryExpr =
      IsSuperset(term, term2)

    def overlaps[U](term2: Term[U])(implicit canOverlap: CanOverlap[T, U]): BinaryExpr =
      Overlaps(term, term2)

    def in[U](term2: Term[U])(implicit canBeIncluded: CanBeIncluded[T, U]): BinaryExpr =
      IsIncluded(term, term2)
  }
}
