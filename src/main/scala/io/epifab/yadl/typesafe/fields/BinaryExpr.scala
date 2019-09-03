package io.epifab.yadl.typesafe.fields

import io.epifab.yadl.typesafe.fields

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

case class Equals[T, U](field1: Field[T], field2: Field[U])(implicit comparable: fields.Comparable[T, U])
  extends BinaryExpr

case class NotEquals[T, U](field1: Field[T], field2: Field[U])(implicit comparable: fields.Comparable[T, U])
  extends BinaryExpr

case class GreaterThan[T, U](field1: Field[T], field2: Field[U])(implicit comparable: fields.Comparable[T, U])
  extends BinaryExpr

case class LessThan[T, U](field1: Field[T], field2: Field[U])(implicit comparable: fields.Comparable[T, U])
  extends BinaryExpr

case class GreaterThanOrEqual[T, U](field1: Field[T], field2: Field[U])(implicit comparable: fields.Comparable[T, U])
  extends BinaryExpr

case class LessThanOrEqual[T, U](field1: Field[T], field2: Field[U])(implicit comparable: fields.Comparable[T, U])
  extends BinaryExpr

case class Like[T, U](field1: Field[T], field2: Field[U])(implicit text1: IsText[T], text2: IsText[U])
  extends BinaryExpr

case class IsSubset[T, U](field1: Field[T], field2: Field[U])(implicit canBeContained: CanBeSubset[T, U])
  extends BinaryExpr

case class IsSuperset[T, U](field1: Field[T], field2: Field[U])(implicit canContain: CanBeSuperset[T, U])
  extends BinaryExpr

case class Overlaps[T, U](field1: Field[T], field2: Field[U])(implicit canOverlap: CanOverlap[T, U])
  extends BinaryExpr

case class IsIncluded[T, U](field1: Field[T], field2: Field[U])(implicit canBeIncluded: CanBeIncluded[T, U])
  extends BinaryExpr

case class IsDefined[T](field: Field[Option[T]]) extends BinaryExpr
case class IsNotDefined[T](field: Field[Option[T]]) extends BinaryExpr
