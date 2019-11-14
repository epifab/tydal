package io.epifab.tydal.fields

import io.epifab.tydal.{Select, Tag, Tagging}
import shapeless.{::, HList, HNil}

import scala.annotation.implicitNotFound

sealed trait Field[+T] {
  def decoder: FieldDecoder[T]
  def as[ALIAS <: Tag](alias: ALIAS): Field[T] with Tagging[ALIAS]
}

case class Column[+T](name: String, relationAlias: String)(implicit val decoder: FieldDecoder[T]) extends Field[T] {
  override def as[ALIAS <: Tag](alias: ALIAS): Column[T] with Tagging[ALIAS] =
    new Column[T](name, relationAlias) with Tagging[ALIAS] {
      override def tagValue: String = alias
    }
}

case class Aggregation[+F <: Field[_], +U](field: F, dbFunction: DbAggregationFunction[F, U])(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  override def as[ALIAS <: Tag](alias: ALIAS): Aggregation[F, U] with Tagging[ALIAS] =
    new Aggregation(field, dbFunction) with Tagging[ALIAS] {
      override def tagValue: String = alias
    }
}

case class Cast[+F <: Field[_], +U](field: F)(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  override def as[ALIAS <: Tag](alias: ALIAS): Cast[F, U] with Tagging[ALIAS] =
    new Cast(field) with Tagging[ALIAS] {
      override def tagValue: String = alias
    }
}

case class SoftCast[+F <: Field[_], +T] private[fields](field: F)(implicit val decoder: FieldDecoder[T])
  extends Field[T] {
  override def as[ALIAS <: Tag](alias: ALIAS): SoftCast[F, T] with Tagging[ALIAS] =
    new SoftCast[F, T](field) with Tagging[ALIAS] {
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
  override def as[ALIAS <: Tag](alias: ALIAS): FieldExpr1[F, U] with Tagging[ALIAS] =
    new FieldExpr1(field, dbFunction) with Tagging[ALIAS] {
      override def tagValue: String = alias
    }
}

case class FieldExpr2[+F1 <: Field[_], +F2 <: Field[_], +U](field1: F1, field2: F2, dbFunction: DbFunction2[F1, F2, U])(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  override def as[ALIAS <: Tag](alias: ALIAS): FieldExpr2[F1, F2, U] with Tagging[ALIAS] =
    new FieldExpr2(field1, field2, dbFunction) with Tagging[ALIAS] {
      override def tagValue: String = alias
    }
}

trait Placeholder[T] extends Field[T]

class NamedPlaceholder[T] private(val name: String)(implicit val decoder: FieldDecoder[T], val encoder: FieldEncoder[T])
  extends Placeholder[T] {

  def as[ALIAS <: Tag](newName: ALIAS): NamedPlaceholder[T] with Tagging[ALIAS] =
    new NamedPlaceholder[T](newName) with Tagging[ALIAS] {
      override def tagValue: String = newName
    }

  override def equals(obj: Any): Boolean = obj match {
    case p: NamedPlaceholder[T] => p.name == name
    case _ => false
  }

  override def toString: String = s"Placeholder($name)"
}

object NamedPlaceholder {
  def apply[TYPE, NAME <: Tag]
  (implicit
   name: ValueOf[NAME],
   encoder: FieldEncoder[TYPE],
   decoder: FieldDecoder[TYPE]): NamedPlaceholder[TYPE] with Tagging[NAME] =
    new NamedPlaceholder(name.value)(decoder, encoder) with Tagging[NAME] {
      override def tagValue: String = name
    }
}

class PlaceholderValue[X](val value: X)(implicit val decoder: FieldDecoder[X], val encoder: FieldEncoder[X])
  extends Placeholder[X] {
  def dbValue: encoder.DBTYPE = encoder.encode(value)

  override def as[ALIAS <: Tag](alias: ALIAS): PlaceholderValue[X] with Tagging[ALIAS] =
    new PlaceholderValue[X](value) with Tagging[ALIAS] {
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
  override def as[ALIAS <: Tag](alias: ALIAS): PlaceholderValueOption[T] with Tagging[ALIAS] = new PlaceholderValueOption(value) with Tagging[ALIAS] {
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
  implicit def pure[TYPE, NAME <: Tag](implicit decoder: FieldDecoder[TYPE], name: ValueOf[NAME]): ColumnsBuilder[Column[TYPE] with Tagging[NAME]] =
    (ds: String) => new Column[TYPE](name.value, ds) with Tagging[NAME] {
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

    def ===[NAME <: Tag, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[T],
     fieldDecoder: FieldDecoder[T],
     comparable: AreComparable[F1, NamedPlaceholder[T] with Tagging[NAME]]): Equals[F1, NamedPlaceholder[T] with Tagging[NAME]] =
      Equals(field1, NamedPlaceholder[T, NAME])

    def like[NAME <: Tag]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     isText: IsText[F1],
     fieldEncoder: FieldEncoder[String],
     fieldDecoder: FieldDecoder[String]): Like[F1, NamedPlaceholder[String] with Tagging[NAME]] =
      Like(field1, NamedPlaceholder[String, NAME])

    def !==[NAME <: Tag, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[T],
     fieldDecoder: FieldDecoder[T],
     comparable: AreComparable[F1, NamedPlaceholder[T] with Tagging[NAME]]): NotEquals[F1, NamedPlaceholder[T] with Tagging[NAME]] =
      NotEquals(field1, NamedPlaceholder[T, NAME])

    def <[NAME <: Tag, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[T],
     fieldDecoder: FieldDecoder[T],
     comparable: AreComparable[F1, NamedPlaceholder[T] with Tagging[NAME]]): LessThan[F1, NamedPlaceholder[T] with Tagging[NAME]] =
      LessThan(field1, NamedPlaceholder[T, NAME])

    def >[NAME <: Tag, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[T],
     fieldDecoder: FieldDecoder[T],
     comparable: AreComparable[F1, NamedPlaceholder[T] with Tagging[NAME]]): GreaterThan[F1, NamedPlaceholder[T] with Tagging[NAME]] =
      GreaterThan(field1, NamedPlaceholder[T, NAME])

    def <=[NAME <: Tag, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[T],
     fieldDecoder: FieldDecoder[T],
     comparable: AreComparable[F1, NamedPlaceholder[T] with Tagging[NAME]]): LessThanOrEqual[F1, NamedPlaceholder[T] with Tagging[NAME]] =
      LessThanOrEqual(field1, NamedPlaceholder[T, NAME])

    def >=[NAME <: Tag, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[T],
     fieldDecoder: FieldDecoder[T],
     comparable: AreComparable[F1, NamedPlaceholder[T] with Tagging[NAME]]): GreaterThanOrEqual[F1, NamedPlaceholder[T] with Tagging[NAME]] = GreaterThanOrEqual(field1, NamedPlaceholder[T, NAME])

    def subsetOf[NAME <: Tag, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[T],
     fieldDecoder: FieldDecoder[T],
     areComparableSeq: AreComparableSeq[F1, NamedPlaceholder[T] with Tagging[NAME]]): IsSubset[F1, NamedPlaceholder[T] with Tagging[NAME]] =
      IsSubset(field1, NamedPlaceholder[T, NAME])

    def supersetOf[NAME <: Tag, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[T],
     fieldDecoder: FieldDecoder[T],
     areComparableSeq: AreComparableSeq[F1, NamedPlaceholder[T] with Tagging[NAME]]): IsSuperset[F1, NamedPlaceholder[T] with Tagging[NAME]] =
      IsSuperset(field1, NamedPlaceholder[T, NAME])

    def overlaps[NAME <: Tag, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[T],
     fieldDecoder: FieldDecoder[T],
     areComparableSeq: AreComparableSeq[F1, NamedPlaceholder[T] with Tagging[NAME]]): Overlaps[F1, NamedPlaceholder[T] with Tagging[NAME]] =
      Overlaps(field1, NamedPlaceholder[T, NAME])

    def in[NAME <: Tag, T]
    (placeholderName: NAME)
    (implicit
     valueOf: ValueOf[NAME],
     fieldT: FieldT[F1, T],
     fieldEncoder: FieldEncoder[Seq[T]],
     fieldDecoder: FieldDecoder[Seq[T]],
     canBeIncluded: CanBeIncluded[F1, NamedPlaceholder[Seq[T]] with Tagging[NAME]]): IsIncluded[F1, NamedPlaceholder[Seq[T]] with Tagging[NAME]] =
      IsIncluded(field1, NamedPlaceholder[Seq[T], NAME])
  }
}
