package io.epifab.yadl.domain.typesafe

import io.epifab.yadl.domain.{DbFunction1, DbFunction2, FieldAdapter}
import shapeless.HList

sealed trait Term[T] extends Taggable {
  def adapter: FieldAdapter[T]

  def castTo[U](implicit adapter: FieldAdapter[U]): Cast[T, U] = Cast(this)
}

final case class Column[T](name: String, dataSource: DataSource[_ <: HList])(implicit val adapter: FieldAdapter[T])
  extends Term[T]

final case class Aggregation[T, U](term: Term[T], dbFunction: AggregateFunction[T, U])(implicit val adapter: FieldAdapter[U])
  extends Term[U]

final case class Cast[T, U](term: Term[T])(implicit val adapter: FieldAdapter[U])
  extends Term[U]

final case class TermExpr1[T, U](term: Term[T], dbFunction: DbFunction1[T, U])(implicit val adapter: FieldAdapter[U])
  extends Term[U]

final case class TermExpr2[T1, T2, U](term1: Term[T1], term2: Term[T2], dbFunction: DbFunction2[T1, T2, U])(implicit val adapter: FieldAdapter[U])
  extends Term[U]

final class Placeholder[T](implicit val adapter: FieldAdapter[T])
  extends Term[T]

object Term {
  def apply[T](name: String, dataSource: DataSource[_ <: HList])(implicit adapter: FieldAdapter[T]): Column[T] =
    Column(name, dataSource)

  def apply[T, U](term: Term[T], dbFunction: AggregateFunction[T, U])(implicit adapter: FieldAdapter[U]): Aggregation[T, U] =
    Aggregation(term, dbFunction)
}
