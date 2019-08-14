package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields.{FieldDecoder, FieldEncoder}
import shapeless.{::, HList, HNil}

sealed trait Term[+T] extends Taggable {
  def decoder: FieldDecoder[T]
  def castTo[U](implicit adapter: FieldDecoder[U]): Cast[T, U] = Cast(this)
}

final case class Column[+T](name: String, dataSource: DataSource[_ <: HList])(implicit val decoder: FieldDecoder[T])
  extends Term[T]

final case class Aggregation[+T, +U](term: Term[T], dbFunction: AggregateFunction[T, U])(implicit val decoder: FieldDecoder[U])
  extends Term[U]

final case class Cast[+T, +U](term: Term[T])(implicit val decoder: FieldDecoder[U])
  extends Term[U]

final case class TermExpr1[+T, +U](term: Term[T], dbFunction: DbFunction1[T, U])(implicit val decoder: FieldDecoder[U])
  extends Term[U]

final case class TermExpr2[+T1, +T2, +U](term1: Term[T1], term2: Term[T2], dbFunction: DbFunction2[T1, T2, U])(implicit val decoder: FieldDecoder[U])
  extends Term[U]

final class Placeholder[+T, -U](implicit val decoder: FieldDecoder[T], val encoder: FieldEncoder[U])
  extends Term[T]

object Term {
  def apply[T](name: String, dataSource: DataSource[_ <: HList])(implicit adapter: FieldDecoder[T]): Column[T] =
    Column(name, dataSource)

  def apply[T, U](term: Term[T], dbFunction: AggregateFunction[T, U])(implicit adapter: FieldDecoder[U]): Aggregation[T, U] =
    Aggregation(term, dbFunction)
}

trait TermsBuilder[+X] {
  def build[DS <: DataSource[_ <: HList]](ds: DS): X
}

object TermsBuilder {
  implicit val hNil: TermsBuilder[HNil] = new TermsBuilder[HNil] {
    override def build[DS <: DataSource[_ <: HList]](ds: DS): HNil = HNil
  }

  implicit def hCons[HT, HA <: String, T <: HList]
    (implicit
     tailTerms: TermsBuilder[T],
     fieldAdapter: FieldDecoder[HT],
     valueOf: ValueOf[HA]): TermsBuilder[(Term[HT] AS HA) :: T] = new TermsBuilder[(Term[HT] AS HA) :: T] {

    override def build[DS <: DataSource[_ <: HList]](ds: DS): (Term[HT] with Tag[HA]) :: T =
      new Column[HT](valueOf.value, ds).as[HA] :: tailTerms.build(ds)
  }
}
