package io.epifab.yadl.typesafe.fields

import io.epifab.yadl.typesafe._
import shapeless.{::, HList, HNil}

sealed trait Field[+T] {
  def decoder: FieldDecoder[T]
  def castTo[U](implicit adapter: FieldDecoder[U]): Cast[T, U] = Cast(this)
  def as[TAG <: String](implicit alias: ValueOf[TAG]): Field[T] with Tag[TAG]
}

case class Column[+T](name: String, dataSource: DataSource[_])(implicit val decoder: FieldDecoder[T])
  extends Field[T] {
  override def as[TAG <: String](implicit alias: ValueOf[TAG]): Column[T] with Tag[TAG] =
    new Column[T](name, dataSource) with Tag[TAG] {
      override def tagValue: String = alias.value
    }
}

case class Aggregation[+T, +U](field: Field[T], dbFunction: DbAggregationFunction[T, U])(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  override def as[TAG <: String](implicit alias: ValueOf[TAG]): Aggregation[T, U] with Tag[TAG] =
    new Aggregation(field, dbFunction) with Tag[TAG] {
      override def tagValue: String = alias.value
    }
}

case class Cast[+T, +U](field: Field[T])(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  override def as[TAG <: String](implicit alias: ValueOf[TAG]): Cast[T, U] with Tag[TAG] =
    new Cast(field) with Tag[TAG] {
      override def tagValue: String = alias.value
    }
}

case class FieldExpr1[+T, +U](field: Field[T], dbFunction: DbFunction1[T, U])(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  override def as[TAG <: String](implicit alias: ValueOf[TAG]): FieldExpr1[T, U] with Tag[TAG] =
    new FieldExpr1[T, U](field, dbFunction) with Tag[TAG] {
      override def tagValue: String = alias.value
    }
}

case class FieldExpr2[+T1, +T2, +U](field1: Field[T1], field2: Field[T2], dbFunction: DbFunction2[T1, T2, U])(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  override def as[TAG <: String](implicit alias: ValueOf[TAG]): FieldExpr2[T1, T2, U] with Tag[TAG] =
    new FieldExpr2(field1, field2, dbFunction) with Tag[TAG] {
      override def tagValue: String = alias.value
    }
}

case class FieldExpr3[+T1, +T2, +T3, +U](field1: Field[T1], field2: Field[T2], field3: Field[T3], dbFunction: DbFunction2[T1, T2, U])(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  override def as[TAG <: String](implicit alias: ValueOf[TAG]): FieldExpr3[T1, T2, T3, U] with Tag[TAG] =
    new FieldExpr3(field1, field2, field3, dbFunction) with Tag[TAG] {
      override def tagValue: String = alias.value
    }
}

class Placeholder[+T, -U](implicit val decoder: FieldDecoder[T], val encoder: FieldEncoder[U])
  extends Field[T] {
  def as[TAG <: String](implicit alias: ValueOf[TAG]): Placeholder[T, U] with Tag[TAG] =
    new Placeholder[T, U] with Tag[TAG] {
      override def tagValue: String = alias.value
    }
}

trait ColumnsBuilder[+X] {
  def build[DS <: DataSource[_]](ds: DS): X
}

object ColumnsBuilder {
  implicit def pure[TYPE, NAME <: String](implicit decoder: FieldDecoder[TYPE], name: ValueOf[NAME]): ColumnsBuilder[Column[TYPE] with Tag[NAME]] = new ColumnsBuilder[Column[TYPE] with Tag[NAME]] {
    override def build[DS <: DataSource[_]](ds: DS): Column[TYPE] with Tag[NAME] = new Column[TYPE](name.value, ds) with Tag[NAME] {
      override def tagValue: String = name
    }
  }

  implicit val hNil: ColumnsBuilder[HNil] = new ColumnsBuilder[HNil] {
    override def build[DS <: DataSource[_]](ds: DS): HNil = HNil
  }

  implicit def hCons[H, T <: HList]
    (implicit
     headTerm: ColumnsBuilder[H],
     tailTerms: ColumnsBuilder[T]): ColumnsBuilder[H :: T] = new ColumnsBuilder[H :: T] {

    override def build[DS <: DataSource[_]](ds: DS): H :: T =
      headTerm.build(ds) :: tailTerms.build(ds)
  }
}
