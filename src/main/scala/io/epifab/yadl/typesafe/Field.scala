package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields.{FieldDecoder, FieldEncoder}
import shapeless.{::, HList, HNil}

sealed trait Field[+T] {
  def decoder: FieldDecoder[T]
  def castTo[U](implicit adapter: FieldDecoder[U]): Cast[T, U] = Cast(this)
  def as[TAG]: Field[T] with Tag[TAG]
}

case class Column[+T](name: String, dataSource: DataSource[_ <: HList])(implicit val decoder: FieldDecoder[T])
  extends Field[T] {
  def as[TAG]: Column[T] with Tag[TAG] = new Column[T](name, dataSource) with Tag[TAG]
}

case class Aggregation[+T, +U](field: Field[T], dbFunction: AggregateFunction[T, U])(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  def as[TAG]: Aggregation[T, U] with Tag[TAG] = new Aggregation(field, dbFunction) with Tag[TAG]
}

case class Cast[+T, +U](field: Field[T])(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  def as[TAG]: Cast[T, U] with Tag[TAG] = new Cast(field) with Tag[TAG]
}

case class FieldExpr1[+T, +U](field: Field[T], dbFunction: DbFunction1[T, U])(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  def as[TAG]: FieldExpr1[T, U] with Tag[TAG] = new FieldExpr1[T, U](field, dbFunction) with Tag[TAG]
}

case class FieldExpr2[+T1, +T2, +U](field1: Field[T1], field2: Field[T2], dbFunction: DbFunction2[T1, T2, U])(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  def as[TAG]: FieldExpr2[T1, T2, U] with Tag[TAG] = new FieldExpr2(field1, field2, dbFunction) with Tag[TAG]
}

class Placeholder[+T, -U](implicit val decoder: FieldDecoder[T], val encoder: FieldEncoder[U])
  extends Field[T] {
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
     valueOf: ValueOf[HA]): ColumnsBuilder[(Field[HT] AS HA) :: T] = new ColumnsBuilder[(Field[HT] AS HA) :: T] {

    override def build[DS <: DataSource[_ <: HList]](ds: DS): (Field[HT] with Tag[HA]) :: T =
      new Column[HT](valueOf.value, ds).as[HA] :: tailTerms.build(ds)
  }
}
