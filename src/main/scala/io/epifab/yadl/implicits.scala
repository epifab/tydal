package io.epifab.yadl

import io.epifab.yadl.domain._
import cats.Applicative
import cats.implicits._

import scala.language.higherKinds
import scala.language.implicitConversions

object implicits {
  implicit class ExtendedSelect[F[_], V, C](select: Select[V])(implicit queryRunner: QueryRunner[F], a: Applicative[F]) {
    def fetchOne: F[Either[DALError, Option[V]]] =
      fetchMany.map(_.map(_.headOption))

    def fetchMany: F[Either[DALError, Seq[V]]] =
      queryRunner.run(select)
  }

  implicit class ExtendedStatementWithSideEffects[F[_]](statement: Statement with SideEffect)(implicit queryRunner: QueryRunner[F]) {
    def execute(): F[Either[DALError, Int]] =
      queryRunner.run(statement)
  }

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

  implicit class ExtendedOptionalTerm[T](term: Term[Option[T]]) {
    def isDefined: BinaryExpr = IsDefined(term)
    def isNotDefined: BinaryExpr = IsNotDefined(term)
  }

  implicit class ExtendedTextLikeTerm[T](term: Term[T])(implicit textLike: TextLike[T]) {
    def `like`[U](term2: Term[U])(implicit textLike2: TextLike[U]): BinaryExpr =
      Like(term, term2)
  }
}
