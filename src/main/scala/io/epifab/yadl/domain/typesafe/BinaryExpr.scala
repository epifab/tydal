package io.epifab.yadl.domain.typesafe

sealed trait BinaryExpr {
  def and(otherExpression: BinaryExpr): BinaryExpr = And(this, otherExpression)
  def or(otherExpression: BinaryExpr): BinaryExpr = Or(this, otherExpression)
}

object BinaryExpr {
  val empty: BinaryExpr = AlwaysTrue
}

case object AlwaysTrue extends BinaryExpr {
  override def and(otherExpression: BinaryExpr): BinaryExpr = otherExpression
}

case class And(expr1: BinaryExpr, expr2: BinaryExpr)
  extends BinaryExpr

case class Or(expr1: BinaryExpr, expr2: BinaryExpr)
  extends BinaryExpr

case class Equals[T, U](term1: Term[T], term2: Term[U])(implicit comparable: Comparable[T, U])
  extends BinaryExpr

case class NotEquals[T, U](term1: Term[T], term2: Term[U])(implicit comparable: Comparable[T, U])
  extends BinaryExpr

case class GreaterThan[T, U](term1: Term[T], term2: Term[U])(implicit comparable: Comparable[T, U])
  extends BinaryExpr

case class LessThan[T, U](term1: Term[T], term2: Term[U])(implicit comparable: Comparable[T, U])
  extends BinaryExpr

case class GreaterThanOrEqual[T, U](term1: Term[T], term2: Term[U])(implicit comparable: Comparable[T, U])
  extends BinaryExpr

case class LessThanOrEqual[T, U](term1: Term[T], term2: Term[U])(implicit comparable: Comparable[T, U])
  extends BinaryExpr

case class Like[T, U](term1: Term[T], term2: Term[U])(implicit text1: IsText[T], text2: IsText[U])
  extends BinaryExpr

case class IsSubset[T, U](term1: Term[T], term2: Term[U])(implicit canBeContained: CanBeSubset[T, U])
  extends BinaryExpr

case class IsSuperset[T, U](term1: Term[T], term2: Term[U])(implicit canContain: CanBeSuperset[T, U])
  extends BinaryExpr

case class Overlaps[T, U](term1: Term[T], term2: Term[U])(implicit canOverlap: CanOverlap[T, U])
  extends BinaryExpr

case class IsIncluded[T, U](term1: Term[T], term2: Term[U])(implicit canBeIncluded: CanBeIncluded[T, U])
  extends BinaryExpr

case class IsDefined[T](term: Term[Option[T]]) extends BinaryExpr
case class IsNotDefined[T](term: Term[Option[T]]) extends BinaryExpr
