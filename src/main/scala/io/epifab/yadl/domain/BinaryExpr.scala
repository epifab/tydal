package io.epifab.yadl.domain

sealed trait Comparable[T, U]

object Comparable {
  implicit def pure[T]: Comparable[T, T] = new Comparable[T, T] {}

  implicit def optionalRight[T, U](implicit comparable: Comparable[T, U]): Comparable[T, Option[U]] =
    new Comparable[T, Option[U]] {}

  implicit def optionalLeft[T, U](implicit comparable: Comparable[T, U]): Comparable[Option[T], U] =
    new Comparable[Option[T], U] {}
}

sealed trait BooleanLike[T]

object BooleanLike {
  implicit val pure: BooleanLike[Boolean] = new BooleanLike[Boolean] {}

  implicit def optional[T](implicit booleanLike: BooleanLike[T]): BooleanLike[T] =
    new BooleanLike[T] {}
}

sealed trait TextLike[T]

object TextLike {
  implicit val pure: TextLike[String] = new TextLike[String] {}

  implicit def optional[T](implicit textLike: TextLike[T]): TextLike[Option[T]] =
    new TextLike[Option[T]] {}
}

sealed trait CanBeSuperset[T, U]

object CanBeSuperset {
  implicit def pure[T]: CanBeSuperset[Seq[T], Seq[T]] = new CanBeSuperset[Seq[T], Seq[T]] {}
}

sealed trait CanBeSubset[T, U]

object CanBeSubset {
  implicit def pure[T]: CanBeSubset[Seq[T], Seq[T]] = new CanBeSubset[Seq[T], Seq[T]] {}
}

sealed trait CanOverlap[T, U]

object CanOverlap {
  implicit def pure[T]: CanOverlap[Seq[T], Seq[T]] = new CanOverlap[Seq[T], Seq[T]] {}
}

sealed trait CanBeIncluded[T, U]

object CanBeIncluded {
  implicit def pure[T]: CanBeIncluded[T, Seq[T]] = new CanBeIncluded[T, Seq[T]] {}
  implicit def optional[T]: CanBeIncluded[Option[T], Seq[T]] = new CanBeIncluded[Option[T], Seq[T]] {}
}

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

case class Like[T, U](term1: Term[T], term2: Term[U])(implicit text1: TextLike[T], text2: TextLike[U])
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