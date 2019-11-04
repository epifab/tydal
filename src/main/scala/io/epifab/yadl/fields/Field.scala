package io.epifab.yadl.fields

import io.epifab.yadl.Tag
import shapeless._

import scala.annotation.implicitNotFound

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

trait Placeholder[T] extends Field[T]

class NamedPlaceholder[T] private(val name: String)(implicit val decoder: FieldDecoder[T], val encoder: FieldEncoder[T])
  extends Placeholder[T] {

  def as[TAG <: String](implicit newName: ValueOf[TAG]): NamedPlaceholder[T] with Tag[TAG] =
    new NamedPlaceholder[T](newName.value) with Tag[TAG] {
      override def tagValue: String = newName.value
    }

  override def equals(obj: Any): Boolean = obj match {
    case p: NamedPlaceholder[T] => p.name == name
    case _ => false
  }

  override def toString: String = s"Placeholder($name)"
}

object NamedPlaceholder {
  def apply[TYPE, NAME <: String]
  (implicit
   name: ValueOf[NAME],
   encoder: FieldEncoder[TYPE],
   decoder: FieldDecoder[TYPE]): NamedPlaceholder[TYPE] with Tag[NAME] =
    new NamedPlaceholder(name.value)(decoder, encoder) with Tag[NAME] {
      override def tagValue: String = name
    }
}

class Value[X](val value: X)(implicit val decoder: FieldDecoder[X], val encoder: FieldEncoder[X])
  extends Placeholder[X] {
  def dbValue: encoder.DBTYPE = encoder.encode(value)

  override def as[TAG <: String](implicit alias: ValueOf[TAG]): Value[X] with Tag[TAG] =
    new Value[X](value) with Tag[TAG] {
      override def tagValue: String = alias.value
    }
}

object Value {
  def apply[TAG <: String with Singleton, VALUE]
  (tag: TAG, value: VALUE)
  (implicit
   encoder: FieldEncoder[VALUE],
   decoder: FieldDecoder[VALUE]): Value[VALUE] with Tag[TAG] =
    new Value(value) with Tag[TAG] {
      override def tagValue: String = tag
    }
}

class OptionalValue[T] private(val value: Option[Value[T]])(implicit val decoder: FieldDecoder[Option[T]], val encoder: FieldEncoder[T])
  extends Placeholder[Option[T]] {
  override def as[TAG <: String](implicit alias: ValueOf[TAG]): OptionalValue[T] with Tag[TAG] = new OptionalValue(value) with Tag[TAG] {
    override def tagValue: String = alias.value
  }
}

object OptionalValue {
  def apply[T]
  (value: Option[T])
  (implicit
   decoderOpt: FieldDecoder[Option[T]],
   decoder: FieldDecoder[T],
   encoder: FieldEncoder[T]): OptionalValue[T] =
    new OptionalValue(value.map(new Value(_)))
}

@implicitNotFound("Could not build a schema for this table.\n" +
  " Possible reasons:\n" +
  " - your schema is invalid: ensure that all properties in your schema are of type Column[T] with Tag[A]" +
  " for concrete T and A;\n" +
  " - a FieldEncoder could not be found for one or more columns in your schema." +
  " Ensure that a FieldEncoder[T] is in scope for every Column[T] in your schema.")
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

trait FieldT[-F <: Field[_], T] {
  def get(f: F): Field[T]
}

object FieldT {
  implicit def pure[T]: FieldT[Field[T], T] = (field: Field[T]) => field
}

trait ValueT[-F <: Value[_], T] {
  def get(f: F): Value[T]
}

object ValueT {
  implicit def pure[T]: ValueT[Value[T], T] = (value: Value[T]) => value
}

object Field {
  implicit class ExtendedField[F1 <: Field[_]](field1: F1) {
    def ===[F2 <: Field[_]](field2: F2)(implicit comparable: AreComparable[F1, F2]): Equals[F1, F2] =
      Equals(field1, field2)

    def !==[F2 <: Field[_]](field2: F2)(implicit comparable: AreComparable[F1, F2]): NotEquals[F1, F2] =
      NotEquals(field1, field2)

    def <[F2 <: Field[_]](field2: F2)(implicit comparable: AreComparable[F1, F2]): LessThan[F1, F2] =
      LessThan(field1, field2)

    def >[F2 <: Field[_]](field2: F2)(implicit comparable: AreComparable[F1, F2]): GreaterThan[F1, F2] =
      GreaterThan(field1, field2)

    def <=[F2 <: Field[_]](field2: F2)(implicit comparable: AreComparable[F1, F2]): LessThanOrEqual[F1, F2] =
      LessThanOrEqual(field1, field2)

    def >=[F2 <: Field[_]](field2: F2)(implicit comparable: AreComparable[F1, F2]): GreaterThanOrEqual[F1, F2] =
      GreaterThanOrEqual(field1, field2)

    def subsetOf[F2 <: Field[_]](field2: F2)(implicit canBeSubset: CanBeSubset[F1, F2]): IsSubset[F1, F2] =
      IsSubset(field1, field2)

    def supersetOf[F2 <: Field[_]](field2: F2)(implicit canBeSuperset: CanBeSuperset[F1, F2]): IsSuperset[F1, F2] =
      IsSuperset(field1, field2)

    def overlaps[F2 <: Field[_]](field2: F2)(implicit canOverlap: CanOverlap[F1, F2]): Overlaps[F1, F2] =
      Overlaps(field1, field2)

    def in[F2 <: Field[_]](field2: F2)(implicit canBeIncluded: CanBeIncluded[F1, F2]): IsIncluded[F1, F2] =
      IsIncluded(field1, field2)

    def ===[NAME <: String with Singleton, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[T],
     fieldDecoder: FieldDecoder[T],
     comparable: AreComparable[F1, NamedPlaceholder[T] with Tag[NAME]]): Equals[F1, NamedPlaceholder[T] with Tag[NAME]] =
      Equals(field1, NamedPlaceholder[T, NAME])

    def !==[NAME <: String with Singleton, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[T],
     fieldDecoder: FieldDecoder[T],
     comparable: AreComparable[F1, NamedPlaceholder[T] with Tag[NAME]]): NotEquals[F1, NamedPlaceholder[T] with Tag[NAME]] =
      NotEquals(field1, NamedPlaceholder[T, NAME])

    def <[NAME <: String with Singleton, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[T],
     fieldDecoder: FieldDecoder[T],
     comparable: AreComparable[F1, NamedPlaceholder[T] with Tag[NAME]]): LessThan[F1, NamedPlaceholder[T] with Tag[NAME]] =
      LessThan(field1, NamedPlaceholder[T, NAME])

    def >[NAME <: String with Singleton, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[T],
     fieldDecoder: FieldDecoder[T],
     comparable: AreComparable[F1, NamedPlaceholder[T] with Tag[NAME]]): GreaterThan[F1, NamedPlaceholder[T] with Tag[NAME]] =
      GreaterThan(field1, NamedPlaceholder[T, NAME])

    def <=[NAME <: String with Singleton, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[T],
     fieldDecoder: FieldDecoder[T],
     comparable: AreComparable[F1, NamedPlaceholder[T] with Tag[NAME]]): LessThanOrEqual[F1, NamedPlaceholder[T] with Tag[NAME]] =
      LessThanOrEqual(field1, NamedPlaceholder[T, NAME])

    def >=[NAME <: String with Singleton, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[T],
     fieldDecoder: FieldDecoder[T],
     comparable: AreComparable[F1, NamedPlaceholder[T] with Tag[NAME]]): GreaterThanOrEqual[F1, NamedPlaceholder[T] with Tag[NAME]] = GreaterThanOrEqual(field1, NamedPlaceholder[T, NAME])

    def subsetOf[NAME <: String with Singleton, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[T],
     fieldDecoder: FieldDecoder[T],
     canBeSubset: CanBeSubset[F1, NamedPlaceholder[T] with Tag[NAME]]): IsSubset[F1, NamedPlaceholder[T] with Tag[NAME]] =
      IsSubset(field1, NamedPlaceholder[T, NAME])

    def supersetOf[NAME <: String with Singleton, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[T],
     fieldDecoder: FieldDecoder[T],
     canBeSuperset: CanBeSuperset[F1, NamedPlaceholder[T] with Tag[NAME]]): IsSuperset[F1, NamedPlaceholder[T] with Tag[NAME]] =
      IsSuperset(field1, NamedPlaceholder[T, NAME])

    def overlaps[NAME <: String with Singleton, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[T],
     fieldDecoder: FieldDecoder[T],
     canOverlap: CanOverlap[F1, NamedPlaceholder[T] with Tag[NAME]]): Overlaps[F1, NamedPlaceholder[T] with Tag[NAME]] =
      Overlaps(field1, NamedPlaceholder[T, NAME])

    def in[NAME <: String with Singleton, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[Seq[T]],
     fieldDecoder: FieldDecoder[Seq[T]],
     canBeIncluded: CanBeIncluded[F1, NamedPlaceholder[Seq[T]] with Tag[NAME]]): IsIncluded[F1, NamedPlaceholder[Seq[T]] with Tag[NAME]] =
      IsIncluded(field1, NamedPlaceholder[Seq[T], NAME])
  }
}