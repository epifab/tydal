package io.epifab.tydal.fields

import io.epifab.tydal.{Select, Tag}
import shapeless.{::, HList, HNil}

import scala.annotation.implicitNotFound

sealed trait Field[+T] {
  def decoder: FieldDecoder[T]
  def as[TAG <: String with Singleton](alias: TAG): Field[T] with Tag[TAG]
}

case class Column[+T](name: String, relationAlias: String)(implicit val decoder: FieldDecoder[T]) extends Field[T] {
  override def as[TAG <: String with Singleton](alias: TAG): Column[T] with Tag[TAG] =
    new Column[T](name, relationAlias) with Tag[TAG] {
      override def tagValue: String = alias
    }
}

case class Aggregation[+F <: Field[_], +U](field: F, dbFunction: DbAggregationFunction[F, U])(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  override def as[TAG <: String with Singleton](alias: TAG): Aggregation[F, U] with Tag[TAG] =
    new Aggregation(field, dbFunction) with Tag[TAG] {
      override def tagValue: String = alias
    }
}

case class Cast[+F <: Field[_], +U](field: F)(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  override def as[TAG <: String with Singleton](alias: TAG): Cast[F, U] with Tag[TAG] =
    new Cast(field) with Tag[TAG] {
      override def tagValue: String = alias
    }
}

case class SoftCast[+F <: Field[_], +T](field: F)(implicit val decoder: FieldDecoder[T])
  extends Field[T] {
  override def as[TAG <: String with Singleton](alias: TAG): SoftCast[F, T] with Tag[TAG] =
    new SoftCast[F, T](field) with Tag[TAG] {
      override def tagValue: String = alias
    }
}

object Nullable {
  def apply[F <: Field[_], G <: Field[_]]
      (field: F)
      (implicit nullableField: NullableField[F, G]): G =
    nullableField(field)
}

case class FieldExpr1[+F <: Field[_], +U](field: F, dbFunction: DbFunction1[F, U])(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  override def as[TAG <: String with Singleton](alias: TAG): FieldExpr1[F, U] with Tag[TAG] =
    new FieldExpr1(field, dbFunction) with Tag[TAG] {
      override def tagValue: String = alias
    }
}

case class FieldExpr2[+F1 <: Field[_], +F2 <: Field[_], +U](field1: F1, field2: F2, dbFunction: DbFunction2[F1, F2, U])(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  override def as[TAG <: String with Singleton](alias: TAG): FieldExpr2[F1, F2, U] with Tag[TAG] =
    new FieldExpr2(field1, field2, dbFunction) with Tag[TAG] {
      override def tagValue: String = alias
    }
}

trait Placeholder[T] extends Field[T]

class NamedPlaceholder[T] private(val name: String)(implicit val decoder: FieldDecoder[T], val encoder: FieldEncoder[T])
  extends Placeholder[T] {

  def as[TAG <: String with Singleton](newName: TAG): NamedPlaceholder[T] with Tag[TAG] =
    new NamedPlaceholder[T](newName) with Tag[TAG] {
      override def tagValue: String = newName
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

class PlaceholderValue[X](val value: X)(implicit val decoder: FieldDecoder[X], val encoder: FieldEncoder[X])
  extends Placeholder[X] {
  def dbValue: encoder.DBTYPE = encoder.encode(value)

  override def as[TAG <: String with Singleton](alias: TAG): PlaceholderValue[X] with Tag[TAG] =
    new PlaceholderValue[X](value) with Tag[TAG] {
      override def tagValue: String = alias
    }
}

object PlaceholderValue {
  def apply[VALUE]
  (value: VALUE)
  (implicit
   encoder: FieldEncoder[VALUE],
   decoder: FieldDecoder[VALUE]): PlaceholderValue[VALUE] =
    new PlaceholderValue(value)
}

class PlaceholderValueOption[T] private(val value: Option[PlaceholderValue[T]])(implicit val decoder: FieldDecoder[Option[T]], val encoder: FieldEncoder[T])
  extends Placeholder[Option[T]] {
  override def as[TAG <: String with Singleton](alias: TAG): PlaceholderValueOption[T] with Tag[TAG] = new PlaceholderValueOption(value) with Tag[TAG] {
    override def tagValue: String = alias
  }
}

object PlaceholderValueOption {
  def apply[T]
  (value: Option[T])
  (implicit
   decoder: FieldDecoder[T],
   encoder: FieldEncoder[T]): PlaceholderValueOption[T] =
    new PlaceholderValueOption(value.map(new PlaceholderValue(_)))(decoder.toOption, encoder)
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

object Field {
  implicit class ExtendedField[F1 <: Field[_]](field1: F1) {
    def ===[F2 <: Field[_]](field2: F2)(implicit comparable: AreComparable[F1, F2]): Equals[F1, F2] =
      Equals(field1, field2)

    def like[F2 <: Field[_]](field2: F2)(implicit leftIsText: IsText[F1], rightIsText: IsText[F2]): Like[F1, F2] =
      Like(field1, field2)

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

    def subsetOf[F2 <: Field[_]](field2: F2)(implicit areComparableSeq: AreComparableSeq[F1, F2]): IsSubset[F1, F2] =
      IsSubset(field1, field2)

    def supersetOf[F2 <: Field[_]](field2: F2)(implicit areComparableSeq: AreComparableSeq[F1, F2]): IsSuperset[F1, F2] =
      IsSuperset(field1, field2)

    def overlaps[F2 <: Field[_]](field2: F2)(implicit areComparableSeq: AreComparableSeq[F1, F2]): Overlaps[F1, F2] =
      Overlaps(field1, field2)

    def in[F2 <: Field[_]](field2: F2)(implicit canBeIncluded: CanBeIncluded[F1, F2]): IsIncluded[F1, F2] =
      IsIncluded(field1, field2)

    def in[
      PLACEHOLDERS <: HList,
      F2 <: Field[_],
      GROUP_BY <: HList,
      SOURCES <: HList,
      WHERE <: BinaryExpr,
      HAVING <: BinaryExpr,
      SORT_BY <: HList
    ](subQuery: Select[F2 :: HNil, GROUP_BY, SOURCES, WHERE, HAVING, SORT_BY])(implicit areComparable: AreComparable[F1, F2]): InSubquery[F1, F2, GROUP_BY, SOURCES, WHERE, HAVING, SORT_BY] =
      InSubquery(field1, subQuery)

    def ===[NAME <: String with Singleton, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[T],
     fieldDecoder: FieldDecoder[T],
     comparable: AreComparable[F1, NamedPlaceholder[T] with Tag[NAME]]): Equals[F1, NamedPlaceholder[T] with Tag[NAME]] =
      Equals(field1, NamedPlaceholder[T, NAME])

    def like[NAME <: String with Singleton]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     isText: IsText[F1],
     fieldEncoder: FieldEncoder[String],
     fieldDecoder: FieldDecoder[String]): Like[F1, NamedPlaceholder[String] with Tag[NAME]] =
      Like(field1, NamedPlaceholder[String, NAME])

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
     areComparableSeq: AreComparableSeq[F1, NamedPlaceholder[T] with Tag[NAME]]): IsSubset[F1, NamedPlaceholder[T] with Tag[NAME]] =
      IsSubset(field1, NamedPlaceholder[T, NAME])

    def supersetOf[NAME <: String with Singleton, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[T],
     fieldDecoder: FieldDecoder[T],
     areComparableSeq: AreComparableSeq[F1, NamedPlaceholder[T] with Tag[NAME]]): IsSuperset[F1, NamedPlaceholder[T] with Tag[NAME]] =
      IsSuperset(field1, NamedPlaceholder[T, NAME])

    def overlaps[NAME <: String with Singleton, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[T],
     fieldDecoder: FieldDecoder[T],
     areComparableSeq: AreComparableSeq[F1, NamedPlaceholder[T] with Tag[NAME]]): Overlaps[F1, NamedPlaceholder[T] with Tag[NAME]] =
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
