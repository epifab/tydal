package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields.{FieldDecoder, FieldEncoder}
import shapeless.{::, HList, HNil}

sealed trait Term[+T] {
  def decoder: FieldDecoder[T]
  def castTo[U](implicit adapter: FieldDecoder[U]): Cast[T, U] = Cast(this)
  def as[TAG]: Term[T] with Tag[TAG]
}

case class Column[+T](name: String, dataSource: DataSource[_ <: HList])(implicit val decoder: FieldDecoder[T])
  extends Term[T] {
  def as[TAG]: Column[T] with Tag[TAG] = new Column[T](name, dataSource) with Tag[TAG]
}

case class Aggregation[+T, +U](term: Term[T], dbFunction: AggregateFunction[T, U])(implicit val decoder: FieldDecoder[U])
  extends Term[U] {
  def as[TAG]: Aggregation[T, U] with Tag[TAG] = new Aggregation(term, dbFunction) with Tag[TAG]
}

case class Cast[+T, +U](term: Term[T])(implicit val decoder: FieldDecoder[U])
  extends Term[U] {
  def as[TAG]: Cast[T, U] with Tag[TAG] = new Cast(term) with Tag[TAG]
}

case class TermExpr1[+T, +U](term: Term[T], dbFunction: DbFunction1[T, U])(implicit val decoder: FieldDecoder[U])
  extends Term[U] {
  def as[TAG]: TermExpr1[T, U] with Tag[TAG] = new TermExpr1[T, U](term, dbFunction) with Tag[TAG]
}

case class TermExpr2[+T1, +T2, +U](term1: Term[T1], term2: Term[T2], dbFunction: DbFunction2[T1, T2, U])(implicit val decoder: FieldDecoder[U])
  extends Term[U] {
  def as[TAG]: TermExpr2[T1, T2, U] with Tag[TAG] = new TermExpr2(term1, term2, dbFunction) with Tag[TAG]
}

class Placeholder[+T, -U](implicit val decoder: FieldDecoder[T], val encoder: FieldEncoder[U])
  extends Term[T] {
  def as[TAG]: Placeholder[T, U] with Tag[TAG] = new Placeholder[T, U] with Tag[TAG]
}

trait ColumnsBuilder[+X] {
  def build[DS <: DataSource[_ <: HList]](ds: DS): X
}

object ColumnsBuilder {
  implicit val hNil: ColumnsBuilder[HNil] = new ColumnsBuilder[HNil] {
    override def build[DS <: DataSource[_ <: HList]](ds: DS): HNil = HNil
  }

  implicit def hCons[HT, HA <: String, T <: HList]
    (implicit
     tailTerms: ColumnsBuilder[T],
     fieldDecoder: FieldDecoder[HT],
     valueOf: ValueOf[HA]): ColumnsBuilder[(Term[HT] AS HA) :: T] = new ColumnsBuilder[(Term[HT] AS HA) :: T] {

    override def build[DS <: DataSource[_ <: HList]](ds: DS): (Term[HT] with Tag[HA]) :: T =
      new Column[HT](valueOf.value, ds).as[HA] :: tailTerms.build(ds)
  }
}
