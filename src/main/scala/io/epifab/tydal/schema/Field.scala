package io.epifab.tydal.schema

import io.epifab.tydal.{As, Tagged, Tagging}
import io.epifab.tydal.queries.SelectQuery
import shapeless.{::, HList, HNil}

sealed trait Field[+T] {
  def decoder: FieldDecoder[T]
  def as[Alias <: String with Singleton](alias: Alias): Field[T] with Tagging[Alias]
}

case class Column[+T](name: String, relationAlias: String)(implicit val decoder: FieldDecoder[T]) extends Field[T] {
  override def as[Alias <: String with Singleton](alias: Alias): Column[T] with Tagging[Alias] =
    new Column[T](name, relationAlias) with Tagging[Alias] {
      override def tagValue: String = alias
    }
}

case class Aggregation[+F <: Field[_], +U](field: F, dbFunction: DbAggregationFunction[F, U])(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  override def as[Alias <: String with Singleton](alias: Alias): Aggregation[F, U] with Tagging[Alias] =
    new Aggregation(field, dbFunction) with Tagging[Alias] {
      override def tagValue: String = alias
    }
}

case class Cast[+F <: Field[_], +U](field: F)(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  override def as[Alias <: String with Singleton](alias: Alias): Cast[F, U] with Tagging[Alias] =
    new Cast(field) with Tagging[Alias] {
      override def tagValue: String = alias
    }
}

case class SoftCast[+F <: Field[_], +T] private[schema](field: F)(implicit val decoder: FieldDecoder[T])
  extends Field[T] {
  override def as[Alias <: String with Singleton](alias: Alias): SoftCast[F, T] with Tagging[Alias] =
    new SoftCast[F, T](field) with Tagging[Alias] {
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
  override def as[Alias <: String with Singleton](alias: Alias): FieldExpr1[F, U] with Tagging[Alias] =
    new FieldExpr1(field, dbFunction) with Tagging[Alias] {
      override def tagValue: String = alias
    }
}

case class FieldExpr2[+F1 <: Field[_], +F2 <: Field[_], +U](field1: F1, field2: F2, dbFunction: DbFunction2[F1, F2, U])(implicit val decoder: FieldDecoder[U])
  extends Field[U] {
  override def as[Alias <: String with Singleton](alias: Alias): FieldExpr2[F1, F2, U] with Tagging[Alias] =
    new FieldExpr2(field1, field2, dbFunction) with Tagging[Alias] {
      override def tagValue: String = alias
    }
}

trait Placeholder[T] extends Field[T]

class NamedPlaceholder[T] private(val name: String)(implicit val decoder: FieldDecoder[T], val encoder: FieldEncoder[T])
  extends Placeholder[T] {

  def as[Alias <: String with Singleton](newName: Alias): NamedPlaceholder[T] with Tagging[Alias] =
    new NamedPlaceholder[T](newName) with Tagging[Alias] {
      override def tagValue: String = newName
    }

  override def equals(obj: Any): Boolean = obj match {
    case p: NamedPlaceholder[T] => p.name == name
    case _ => false
  }

  override def toString: String = s"Placeholder($name)"
}

object NamedPlaceholder {
  def apply[T, Name <: String with Singleton]
  (implicit
   name: ValueOf[Name],
   encoder: FieldEncoder[T],
   decoder: FieldDecoder[T]): NamedPlaceholder[T] with Tagging[Name] =
    new NamedPlaceholder(name.value)(decoder, encoder) with Tagging[Name] {
      override def tagValue: String = name
    }
}

trait NamedPlaceholders[Fields, Placeholders] {
  def get: Placeholders
}

object NamedPlaceholders {
  implicit def pure[F <: Field[_], T, A <: String with Singleton](
    implicit
    fieldT: FieldT[F, T],
    tagged: Tagged[F, A],
    fieldEncoder: FieldEncoder[T],
    fieldDecoder: FieldDecoder[T],
    alias: ValueOf[A]
  ): NamedPlaceholders[F, NamedPlaceholder[T] As A] = new NamedPlaceholders[F, NamedPlaceholder[T] As A] {
    override def get: As[NamedPlaceholder[T], A] = NamedPlaceholder[T, A]
  }

  implicit val hNil: NamedPlaceholders[HNil, HNil] = new NamedPlaceholders[HNil, HNil] {
    override def get: HNil = HNil
  }

  implicit def hCons[FH, PH, FT <: HList, PT <: HList](
    implicit
    head: NamedPlaceholders[FH, PH],
    tail: NamedPlaceholders[FT, PT]
  ): NamedPlaceholders[FH :: FT, PH :: PT] = new NamedPlaceholders[FH :: FT, PH :: PT] {
    override def get: PH :: PT = head.get :: tail.get
  }
}

class Literal[T](val value: T)(implicit val decoder: FieldDecoder[T], val encoder: FieldEncoder[T])
  extends Placeholder[T] {
  def dbValue: encoder.DbType = encoder.encode(value)

  override def as[Alias <: String with Singleton](alias: Alias): Literal[T] with Tagging[Alias] =
    new Literal[T](value) with Tagging[Alias] {
      override def tagValue: String = alias
    }
}

object Literal {
  def apply[Value](value: Value)(
    implicit
    encoder: FieldEncoder[Value],
    decoder: FieldDecoder[Value]
  ): Literal[Value] =
    new Literal(value)
}

class LiteralOption[T] private(val value: Option[Literal[T]])(implicit val decoder: FieldDecoder[Option[T]], val encoder: FieldEncoder[T])
  extends Placeholder[Option[T]] {
  override def as[Alias <: String with Singleton](alias: Alias): LiteralOption[T] with Tagging[Alias] = new LiteralOption(value) with Tagging[Alias] {
    override def tagValue: String = alias
  }
}

object LiteralOption {
  private[tydal] def apply[T](value: Option[T])(
    implicit
    decoder: FieldDecoder[T],
    encoder: FieldEncoder[T]
  ): LiteralOption[T] =
    new LiteralOption(value.map(new Literal(_)))(decoder.toOption, encoder)
}

trait LiteralOptions[-T, +U] {
  def build(t: T): U
  def empty: U
}

object LiteralOptions {
  implicit def literal[T](implicit fieldEncoder: FieldEncoder[T], fieldDecoder: FieldDecoder[T]): LiteralOptions[Literal[T], LiteralOption[T]] = new LiteralOptions[Literal[T], LiteralOption[T]] {
    override def build(p: Literal[T]): LiteralOption[T] = LiteralOption[T](Some(p.value))
    override def empty: LiteralOption[T] = LiteralOption[T](None)
  }

  implicit def literalOption[T](implicit fieldEncoder: FieldEncoder[T], fieldDecoder: FieldDecoder[T]): LiteralOptions[LiteralOption[T], LiteralOption[T]] =
    new LiteralOptions[LiteralOption[T], LiteralOption[T]] {
      override def build(p: LiteralOption[T]): LiteralOption[T] = p
      override def empty: LiteralOption[T] = LiteralOption(None)
    }

  implicit def namedPlaceholder[T, A <: String with Singleton](
    implicit
    fieldEncoder: FieldEncoder[T],
    fieldDecoder: FieldDecoder[T],
    valueOf: ValueOf[A]
  ): LiteralOptions[NamedPlaceholder[T] with Tagging[A], NamedPlaceholder[T] with Tagging[A]] =
    new LiteralOptions[NamedPlaceholder[T] with Tagging[A], NamedPlaceholder[T] with Tagging[A]] {
      override def build(p: NamedPlaceholder[T] with Tagging[A]): NamedPlaceholder[T] with Tagging[A] = p
      override def empty: NamedPlaceholder[T] with Tagging[A] = NamedPlaceholder[T, A]
    }

  implicit val hNil: LiteralOptions[HNil, HNil] = new LiteralOptions[HNil, HNil] {
    override def build(t: HNil): HNil = HNil
    override def empty: HNil = HNil
  }

  implicit def hCons[H1, H2, T1 <: HList, T2 <: HList](
    implicit
    head: LiteralOptions[H1, H2],
    tail: LiteralOptions[T1, T2]
  ): LiteralOptions[H1 :: T1, H2 :: T2] = new LiteralOptions[H1 :: T1, H2 :: T2] {
    override def build(p: H1 :: T1): H2 :: T2 = head.build(p.head) :: tail.build(p.tail)
    override def empty: H2 :: T2 = head.empty :: tail.empty
  }
}

trait FieldT[-F <: Field[_], T] {
  def get(f: F): Field[T]
}

object FieldT {
  implicit def pure[T]: FieldT[Field[T], T] = (field: Field[T]) => field
}

object Field {
  implicit class ExtendedNonOptionalField[F1 <: Field[_]](field1: F1)(implicit nonOptional: Negative[IsOptional[F1]]) {
    def castTo[U](implicit fieldDecoder: FieldDecoder[U], notNull: Negative[IsOptional[F1]]): Cast[F1, U] =
      Cast(field1)
  }

  implicit class ExtendedOptionalField[F1 <: Field[_]](field1: F1)(implicit isOptional: IsOptional[F1]) {
    def castTo[U](implicit fieldDecoder: FieldDecoder[U], isOptional: IsOptional[F1]): Cast[F1, Option[U]] =
      Cast(field1)(fieldDecoder.toOption)

    def isDefined: IsDefined[F1] = IsDefined(field1)
    def isNotDefined: IsNotDefined[F1] = IsNotDefined(field1)
  }

  implicit class ExtendedField[F1 <: Field[_]](field1: F1) {
    def ===[F2 <: Field[_]](field2: F2)(implicit comparable: AreComparable[F1, F2]): Equals[F1, F2] =
      Equals(field1, field2)

    def like[F2 <: Field[_]](field2: F2)(implicit leftIsText: IsText[F1], rightIsText: IsText[F2]): Like[F1, F2] =
      Like(field1, field2)

    def ilike[F2 <: Field[_]](field2: F2)(implicit leftIsText: IsText[F1], rightIsText: IsText[F2]): ILike[F1, F2] =
      ILike(field1, field2)

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
      Placeholders <: HList,
      F2 <: Field[_],
      GroupBy <: HList,
      Sources <: HList,
      Where <: Filter,
      Having <: Filter,
      Sort <: HList,
      Offset,
      Limit
    ](subQuery: SelectQuery[F2 :: HNil, GroupBy, Sources, Where, Having, Sort, Offset, Limit])(implicit areComparable: AreComparable[F1, F2]): InSubquery[F1, F2, GroupBy, Sources, Where, Having, Sort, Offset, Limit] =
      InSubquery(field1, subQuery)

    def ===[PlaceholderName <: String with Singleton, T](placeholderName: PlaceholderName)(
      implicit
      valueOf: ValueOf[PlaceholderName],
      fieldT: FieldT[F1, T],
      fieldEncoder: FieldEncoder[T],
      fieldDecoder: FieldDecoder[T],
      comparable: AreComparable[F1, NamedPlaceholder[T] with Tagging[PlaceholderName]]
    ): Equals[F1, NamedPlaceholder[T] with Tagging[PlaceholderName]] =
      Equals(field1, NamedPlaceholder[T, PlaceholderName])

    def like[PlaceholderName <: String with Singleton](placeholderName: PlaceholderName)(
      implicit
      valueOf: ValueOf[PlaceholderName],
      isText: IsText[F1],
      fieldEncoder: FieldEncoder[String],
      fieldDecoder: FieldDecoder[String]
    ): Like[F1, NamedPlaceholder[String] with Tagging[PlaceholderName]] =
      Like(field1, NamedPlaceholder[String, PlaceholderName])

    def ilike[PlaceholderName <: String with Singleton](placeholderName: PlaceholderName)(
      implicit
      valueOf: ValueOf[PlaceholderName],
      isText: IsText[F1],
      fieldEncoder: FieldEncoder[String],
      fieldDecoder: FieldDecoder[String]
    ): ILike[F1, NamedPlaceholder[String] with Tagging[PlaceholderName]] =
      ILike(field1, NamedPlaceholder[String, PlaceholderName])

    def !==[PlaceholderName <: String with Singleton, T](placeholderName: PlaceholderName)(
      implicit
      valueOf: ValueOf[PlaceholderName],
      fieldT: FieldT[F1, T],
      fieldEncoder: FieldEncoder[T],
      fieldDecoder: FieldDecoder[T],
      comparable: AreComparable[F1, NamedPlaceholder[T] with Tagging[PlaceholderName]]
    ): NotEquals[F1, NamedPlaceholder[T] with Tagging[PlaceholderName]] =
      NotEquals(field1, NamedPlaceholder[T, PlaceholderName])

    def <[PlaceholderName <: String with Singleton, T](placeholderName: PlaceholderName)(
      implicit
      valueOf: ValueOf[PlaceholderName],
      fieldT: FieldT[F1, T],
      fieldEncoder: FieldEncoder[T],
      fieldDecoder: FieldDecoder[T],
      comparable: AreComparable[F1, NamedPlaceholder[T] with Tagging[PlaceholderName]]
    ): LessThan[F1, NamedPlaceholder[T] with Tagging[PlaceholderName]] =
      LessThan(field1, NamedPlaceholder[T, PlaceholderName])

    def >[PlaceholderName <: String with Singleton, T](placeholderName: PlaceholderName)(
      implicit
      valueOf: ValueOf[PlaceholderName],
      fieldT: FieldT[F1, T],
      fieldEncoder: FieldEncoder[T],
      fieldDecoder: FieldDecoder[T],
      comparable: AreComparable[F1, NamedPlaceholder[T] with Tagging[PlaceholderName]]
    ): GreaterThan[F1, NamedPlaceholder[T] with Tagging[PlaceholderName]] =
      GreaterThan(field1, NamedPlaceholder[T, PlaceholderName])

    def <=[PlaceholderName <: String with Singleton, T](placeholderName: PlaceholderName)(
      implicit
      valueOf: ValueOf[PlaceholderName],
      fieldT: FieldT[F1, T],
      fieldEncoder: FieldEncoder[T],
      fieldDecoder: FieldDecoder[T],
      comparable: AreComparable[F1, NamedPlaceholder[T] with Tagging[PlaceholderName]]
    ): LessThanOrEqual[F1, NamedPlaceholder[T] with Tagging[PlaceholderName]] =
      LessThanOrEqual(field1, NamedPlaceholder[T, PlaceholderName])

    def >=[PlaceholderName <: String with Singleton, T](placeholderName: PlaceholderName)(
      implicit
      valueOf: ValueOf[PlaceholderName],
      fieldT: FieldT[F1, T],
      fieldEncoder: FieldEncoder[T],
      fieldDecoder: FieldDecoder[T],
      comparable: AreComparable[F1, NamedPlaceholder[T] with Tagging[PlaceholderName]]
    ): GreaterThanOrEqual[F1, NamedPlaceholder[T] with Tagging[PlaceholderName]] =
      GreaterThanOrEqual(field1, NamedPlaceholder[T, PlaceholderName])

    def subsetOf[PlaceholderName <: String with Singleton, T](placeholderName: PlaceholderName)(
      implicit
      valueOf: ValueOf[PlaceholderName],
      fieldT: FieldT[F1, T],
      fieldEncoder: FieldEncoder[T],
      fieldDecoder: FieldDecoder[T],
      areComparableSeq: AreComparableSeq[F1, NamedPlaceholder[T] with Tagging[PlaceholderName]]
    ): IsSubset[F1, NamedPlaceholder[T] with Tagging[PlaceholderName]] =
      IsSubset(field1, NamedPlaceholder[T, PlaceholderName])

    def supersetOf[PlaceholderName <: String with Singleton, T](placeholderName: PlaceholderName)(
      implicit
      valueOf: ValueOf[PlaceholderName],
      fieldT: FieldT[F1, T],
      fieldEncoder: FieldEncoder[T],
      fieldDecoder: FieldDecoder[T],
      areComparableSeq: AreComparableSeq[F1, NamedPlaceholder[T] with Tagging[PlaceholderName]]
    ): IsSuperset[F1, NamedPlaceholder[T] with Tagging[PlaceholderName]] =
      IsSuperset(field1, NamedPlaceholder[T, PlaceholderName])

    def overlaps[PlaceholderName <: String with Singleton, T](placeholderName: PlaceholderName)(
      implicit
      valueOf: ValueOf[PlaceholderName],
      fieldT: FieldT[F1, T],
      fieldEncoder: FieldEncoder[T],
      fieldDecoder: FieldDecoder[T],
      areComparableSeq: AreComparableSeq[F1, NamedPlaceholder[T] with Tagging[PlaceholderName]]
    ): Overlaps[F1, NamedPlaceholder[T] with Tagging[PlaceholderName]] =
      Overlaps(field1, NamedPlaceholder[T, PlaceholderName])

    def in[PlaceholderName <: String with Singleton, T](placeholderName: PlaceholderName)(
      implicit
      valueOf: ValueOf[PlaceholderName],
      fieldT: FieldT[F1, T],
      fieldEncoder: FieldEncoder[Seq[T]],
      fieldDecoder: FieldDecoder[Seq[T]],
      canBeIncluded: CanBeIncluded[F1, NamedPlaceholder[Seq[T]] with Tagging[PlaceholderName]]
    ): IsIncluded[F1, NamedPlaceholder[Seq[T]] with Tagging[PlaceholderName]] =
      IsIncluded(field1, NamedPlaceholder[Seq[T], PlaceholderName])
  }
}
