package io.epifab.tydal.schema

import io.epifab.tydal.queries.SelectQuery
import shapeless.{::, HList, HNil}

sealed trait Filter {
  def and[F2 <: Filter](otherExpression: F2): And[this.type, F2] = And(this, otherExpression)
  def or[F2 <: Filter](otherExpression: F2): Or[this.type, F2] = Or(this, otherExpression)
}

sealed trait AlwaysTrue extends Filter

trait FilterOption[+E <: Filter] extends Filter {
  def filter: Option[E]
}

sealed trait Filter1[+F] extends Filter {
  def field: F
}

sealed trait Filter2[+F1, +F2] extends Filter {
  def left: F1
  def right: F2
}

case object AlwaysTrue extends AlwaysTrue

case class IsDefined[+F <: Field[_]](field: F)(implicit isOptional: IsOptional[F])
  extends Filter1[F]

case class IsNotDefined[+F <: Field[_]](field: F)(implicit isOptional: IsOptional[F])
  extends Filter1[F]

case class And[+F1 <: Filter, +F2 <: Filter](left: F1, right: F2)
  extends Filter2[F1, F2]

case class Or[+F1 <: Filter, +F2 <: Filter](left: F1, right: F2)
  extends Filter2[F1, F2]

case class Equals[+F1 <: Field[_], +F2 <: Field[_]](left: F1, right: F2)(implicit comparable: AreComparable[F1, F2])
  extends Filter2[F1, F2]

case class NotEquals[+F1 <: Field[_], +F2 <: Field[_]](left: F1, right: F2)(implicit comparable: AreComparable[F1, F2])
  extends Filter2[F1, F2]

case class GreaterThan[+F1 <: Field[_], +F2 <: Field[_]](left: F1, right: F2)(implicit comparable: AreComparable[F1, F2])
  extends Filter2[F1, F2]

case class LessThan[+F1 <: Field[_], +F2 <: Field[_]](left: F1, right: F2)(implicit comparable: AreComparable[F1, F2])
  extends Filter2[F1, F2]

case class GreaterThanOrEqual[+F1 <: Field[_], +F2 <: Field[_]](left: F1, right: F2)(implicit comparable: AreComparable[F1, F2])
  extends Filter2[F1, F2]

case class LessThanOrEqual[+F1 <: Field[_], +F2 <: Field[_]](left: F1, right: F2)(implicit comparable: AreComparable[F1, F2])
  extends Filter2[F1, F2]

case class Like[+F1 <: Field[_], +F2 <: Field[_]](left: F1, right: F2)(implicit text1: IsText[F1], text2: IsText[F2])
  extends Filter2[F1, F2]

case class IsSubset[+F1 <: Field[_], +F2 <: Field[_]](left: F1, right: F2)(implicit areComparableSeq: AreComparableSeq[F1, F2])
  extends Filter2[F1, F2]

case class IsSuperset[+F1 <: Field[_], +F2 <: Field[_]](left: F1, right: F2)(implicit areComparableSeq: AreComparableSeq[F1, F2])
  extends Filter2[F1, F2]

case class Overlaps[+F1 <: Field[_], +F2 <: Field[_]](left: F1, right: F2)(implicit areComparableSeq: AreComparableSeq[F1, F2])
  extends Filter2[F1, F2]

case class IsIncluded[+F1 <: Field[_], +F2 <: Field[_]](left: F1, right: F2)(implicit canBeIncluded: CanBeIncluded[F1, F2])
  extends Filter2[F1, F2]

case class InSubquery[+F1 <: Field[_], F2 <: Field[_], GroupBy <: HList, Sources <: HList, Where <: Filter, Having <: Filter, Sort <: HList, Offset, Limit](
  left: F1,
  right: SelectQuery[F2 :: HNil, GroupBy, Sources, Where, Having, Sort, Offset, Limit]
)(implicit areComparable: AreComparable[F1, F2])
  extends Filter2[F1, SelectQuery[F2 :: HNil, GroupBy, Sources, Where, Having, Sort, Offset, Limit]]
