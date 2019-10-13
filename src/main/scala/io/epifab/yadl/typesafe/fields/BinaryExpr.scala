package io.epifab.yadl.typesafe.fields

import io.epifab.yadl.typesafe.fields

sealed trait BinaryExpr {
  def and[E2 <: BinaryExpr](otherExpression: E2): And[this.type, E2] = And(this, otherExpression)
  def or[E2 <: BinaryExpr](otherExpression: E2): Or[this.type, E2] = Or(this, otherExpression)
}

sealed trait BinaryExpr1[E] extends BinaryExpr {
  def expr: E
}

sealed trait BinaryExpr2[E1, E2] extends BinaryExpr {
  def expr1: E1
  def expr2: E2
}

sealed trait AlwaysTrue

case object AlwaysTrue extends AlwaysTrue with BinaryExpr

case class And[E1 <: BinaryExpr, E2 <: BinaryExpr](expr1: E1, expr2: E2)
  extends BinaryExpr2[E1, E2]

case class Or[E1 <: BinaryExpr, E2 <: BinaryExpr](expr1: E1, expr2: E2)
  extends BinaryExpr2[E1, E2]

case class Equals[F1 <: Field[_], F2 <: Field[_]](expr1: F1, expr2: F2)(implicit comparable: fields.Comparable[F1, F2])
  extends BinaryExpr2[F1, F2]

case class NotEquals[F1 <: Field[_], F2 <: Field[_]](expr1: F1, expr2: F2)(implicit comparable: fields.Comparable[F1, F2])
  extends BinaryExpr2[F1, F2]

case class GreaterThan[F1 <: Field[_], F2 <: Field[_]](expr1: F1, expr2: F2)(implicit comparable: fields.Comparable[F1, F2])
  extends BinaryExpr2[F1, F2]

case class LessThan[F1 <: Field[_], F2 <: Field[_]](expr1: F1, expr2: F2)(implicit comparable: fields.Comparable[F1, F2])
  extends BinaryExpr2[F1, F2]

case class GreaterThanOrEqual[F1 <: Field[_], F2 <: Field[_]](expr1: F1, expr2: F2)(implicit comparable: fields.Comparable[F1, F2])
  extends BinaryExpr2[F1, F2]

case class LessThanOrEqual[F1 <: Field[_], F2 <: Field[_]](expr1: F1, expr2: F2)(implicit comparable: fields.Comparable[F1, F2])
  extends BinaryExpr2[F1, F2]

case class Like[F1 <: Field[_], F2 <: Field[_]](expr1: F1, expr2: F2)(implicit text1: IsText[F1], text2: IsText[F2])
  extends BinaryExpr2[F1, F2]

case class IsSubset[F1 <: Field[_], F2 <: Field[_]](expr1: F1, expr2: F2)(implicit canBeContained: CanBeSubset[F1, F2])
  extends BinaryExpr2[F1, F2]

case class IsSuperset[F1 <: Field[_], F2 <: Field[_]](expr1: F1, expr2: F2)(implicit canContain: CanBeSuperset[F1, F2])
  extends BinaryExpr2[F1, F2]

case class Overlaps[F1 <: Field[_], F2 <: Field[_]](expr1: F1, expr2: F2)(implicit canOverlap: CanOverlap[F1, F2])
  extends BinaryExpr2[F1, F2]

case class IsIncluded[F1 <: Field[_], F2 <: Field[_]](expr1: F1, expr2: F2)(implicit canBeIncluded: CanBeIncluded[F1, F2])
  extends BinaryExpr2[F1, F2]

case class IsDefined[F <: Field[Option[_]]](expr: F)
  extends BinaryExpr1[F]

case class IsNotDefined[F <: Field[Option[_]]](expr: F)
  extends BinaryExpr1[F]
