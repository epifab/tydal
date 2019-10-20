package io.epifab.yadl.typesafe.fields

import io.epifab.yadl.typesafe._
import shapeless._

sealed trait Field[+T] {
  def decoder: FieldDecoder[T]
  def as[TAG <: String](implicit alias: ValueOf[TAG]): Field[T] with Tag[TAG]
}

case class Column[+T](name: String, srcAlias: String)(implicit val decoder: FieldDecoder[T]) extends Field[T] {
  override def as[TAG <: String](implicit alias: ValueOf[TAG]): Column[T] with Tag[TAG] =
    new Column[T](name, srcAlias) with Tag[TAG] {
      override def tagValue: String = alias.value
    }
}

case class Aggregation[+F <: Field[_], +U](field: F, dbFunction: DbAggregationFunction[F, U])(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  override def as[TAG <: String](implicit alias: ValueOf[TAG]): Aggregation[F, U] with Tag[TAG] =
    new Aggregation(field, dbFunction) with Tag[TAG] {
      override def tagValue: String = alias.value
    }
}

case class Cast[+F <: Field[_], +U](field: F)(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  override def as[TAG <: String](implicit alias: ValueOf[TAG]): Cast[F, U] with Tag[TAG] =
    new Cast(field) with Tag[TAG] {
      override def tagValue: String = alias.value
    }
}

case class Nullable[+F <: Field[_], +T](field: F)(implicit fieldT: FieldT[F, T], val decoder: FieldDecoder[Option[T]])
  extends Field[Option[T]] {
  override def as[TAG <: String](implicit alias: ValueOf[TAG]): Nullable[F, T] with Tag[TAG] =
    new Nullable[F, T](field) with Tag[TAG] {
      override def tagValue: String = alias.value
    }
}

case class FieldExpr1[+F <: Field[_], +U](field: F, dbFunction: DbFunction1[F, U])(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  override def as[TAG <: String](implicit alias: ValueOf[TAG]): FieldExpr1[F, U] with Tag[TAG] =
    new FieldExpr1(field, dbFunction) with Tag[TAG] {
      override def tagValue: String = alias.value
    }
}

case class FieldExpr2[+F1 <: Field[_], +F2 <: Field[_], +U](field1: F1, field2: F2, dbFunction: DbFunction2[F1, F2, U])(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  override def as[TAG <: String](implicit alias: ValueOf[TAG]): FieldExpr2[F1, F2, U] with Tag[TAG] =
    new FieldExpr2(field1, field2, dbFunction) with Tag[TAG] {
      override def tagValue: String = alias.value
    }
}

class Placeholder[+T, -U] private(val name: String)(implicit val decoder: FieldDecoder[T], val encoder: FieldEncoder[U])
  extends Field[T] { Self =>

  def as[TAG <: String](implicit newName: ValueOf[TAG]): Placeholder[T, U] with Tag[TAG] =
    new Placeholder[T, U](newName.value) with Tag[TAG] {
      override def tagValue: String = newName.value
    }

  override def equals(obj: Any): Boolean = obj match {
    case p: Placeholder[_, _] => p.name == name && p.encoder == encoder && p.decoder == decoder
    case _ => false
  }
}

class PlaceholderValue[+X](val value: X)(implicit fieldEncoder: FieldEncoder[X])

trait NamedPlaceholder[NAME <: String, X] {
  def resolve(x: X): PlaceholderValue[X] with Tag[NAME]
}

object Placeholder {
  def apply[NAME <: String, TYPE]
    (implicit
     name: ValueOf[NAME],
     encoder: FieldEncoder[TYPE],
     decoder: FieldDecoder[TYPE]): Placeholder[TYPE, TYPE] with Tag[NAME] with NamedPlaceholder[NAME, TYPE] =
    new Placeholder(name.value)(decoder, encoder) with Tag[NAME] with NamedPlaceholder[NAME, TYPE] {
      override def tagValue: String = name
      override def resolve(u: TYPE): PlaceholderValue[TYPE] with Tag[NAME] =
        new PlaceholderValue(u) with Tag[NAME] {
          override def tagValue: String = name
        }
    }
}

trait ColumnsBuilder[+X] {
  def build(ds: String): X
}

object ColumnsBuilder {
  implicit def pure[TYPE, NAME <: String](implicit decoder: FieldDecoder[TYPE], name: ValueOf[NAME]): ColumnsBuilder[Column[TYPE] with Tag[NAME]] =
    (ds: String) => new Column[TYPE](name.value, ds) with Tag[NAME] {
      override def tagValue: String = name
    }

  implicit val hNil: ColumnsBuilder[HNil] =
    (ds: String) => HNil

  implicit def hCons[H, T <: HList]
      (implicit
       headTerm: ColumnsBuilder[H],
       tailTerms: ColumnsBuilder[T]): ColumnsBuilder[H :: T] =
    (ds: String) => headTerm.build(ds) :: tailTerms.build(ds)
}

trait FieldT[-F <: Field[_], +T] {
  def get(f: F): Field[T]
}

object FieldT {
  implicit def pure[T]: FieldT[Field[T], T] = (field: Field[T]) => field
}
