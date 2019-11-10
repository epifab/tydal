package io.epifab.tydal.fields

import io.epifab.tydal.Select
import shapeless.{::, HList, HNil}

sealed trait AlwaysTrue extends BinaryExpr

sealed trait BinaryExpr {
  def and[E2 <: BinaryExpr](otherExpression: E2): And[this.type, E2] = And(this, otherExpression)
  def or[E2 <: BinaryExpr](otherExpression: E2): Or[this.type, E2] = Or(this, otherExpression)
}

sealed trait BinaryExpr1[+E] extends BinaryExpr {
  def expr: E
}

sealed trait BinaryExpr2[+E1, +E2] extends BinaryExpr {
  def left: E1
  def right: E2
}

case object AlwaysTrue extends AlwaysTrue

case class IsDefined[+E <: Field[Option[_]]](expr: E)
  extends BinaryExpr1[E]

case class IsNotDefined[+E <: Field[Option[_]]](expr: E)
  extends BinaryExpr1[E]

case class And[+E1 <: BinaryExpr, +E2 <: BinaryExpr](left: E1, right: E2)
  extends BinaryExpr2[E1, E2]

case class Or[+E1 <: BinaryExpr, +E2 <: BinaryExpr](left: E1, right: E2)
  extends BinaryExpr2[E1, E2]

case class Equals[+E1 <: Field[_], +E2 <: Field[_]](left: E1, right: E2)(implicit comparable: AreComparable[E1, E2])
  extends BinaryExpr2[E1, E2]

case class NotEquals[+E1 <: Field[_], +E2 <: Field[_]](left: E1, right: E2)(implicit comparable: AreComparable[E1, E2])
  extends BinaryExpr2[E1, E2]

case class GreaterThan[+E1 <: Field[_], +E2 <: Field[_]](left: E1, right: E2)(implicit comparable: AreComparable[E1, E2])
  extends BinaryExpr2[E1, E2]

case class LessThan[+E1 <: Field[_], +E2 <: Field[_]](left: E1, right: E2)(implicit comparable: AreComparable[E1, E2])
  extends BinaryExpr2[E1, E2]

case class GreaterThanOrEqual[+E1 <: Field[_], +E2 <: Field[_]](left: E1, right: E2)(implicit comparable: AreComparable[E1, E2])
  extends BinaryExpr2[E1, E2]

case class LessThanOrEqual[+E1 <: Field[_], +E2 <: Field[_]](left: E1, right: E2)(implicit comparable: AreComparable[E1, E2])
  extends BinaryExpr2[E1, E2]

case class Like[+E1 <: Field[_], +E2 <: Field[_]](left: E1, right: E2)(implicit text1: IsText[E1], text2: IsText[E2])
  extends BinaryExpr2[E1, E2]

case class IsSubset[+E1 <: Field[_], +E2 <: Field[_]](left: E1, right: E2)(implicit areComparableSeq: AreComparableSeq[E1, E2])
  extends BinaryExpr2[E1, E2]

case class IsSuperset[+E1 <: Field[_], +E2 <: Field[_]](left: E1, right: E2)(implicit areComparableSeq: AreComparableSeq[E1, E2])
  extends BinaryExpr2[E1, E2]

case class Overlaps[+E1 <: Field[_], +E2 <: Field[_]](left: E1, right: E2)(implicit areComparableSeq: AreComparableSeq[E1, E2])
  extends BinaryExpr2[E1, E2]

case class IsIncluded[+E1 <: Field[_], +E2 <: Field[_]](left: E1, right: E2)(implicit canBeIncluded: CanBeIncluded[E1, E2])
  extends BinaryExpr2[E1, E2]

case class InSubquery[+E1 <: Field[_], E2 <: Field[_], GROUP_BY <: HList, SOURCES <: HList, WHERE <: BinaryExpr, HAVING <: BinaryExpr, SORT_BY <: HList](left: E1, right: Select[E2 :: HNil, GROUP_BY, SOURCES, WHERE, HAVING, SORT_BY])(implicit areComparable: AreComparable[E1, E2])
  extends BinaryExpr2[E1, Select[E2 :: HNil, GROUP_BY, SOURCES, WHERE, HAVING, SORT_BY]]
