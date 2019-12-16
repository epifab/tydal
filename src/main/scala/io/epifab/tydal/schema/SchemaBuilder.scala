package io.epifab.tydal.schema

import io.epifab.tydal.Tagging
import io.epifab.tydal.utils.Concat
import shapeless.labelled.FieldType
import shapeless.ops.hlist.Tupler
import shapeless.tag.@@
import shapeless.{::, HList, HNil, LabelledGeneric}

import scala.annotation.implicitNotFound

sealed trait GroupByColumns[-Fields, +Columns <: HList] {
  def apply(fields: Fields): Columns
}

object GroupByColumns {
  implicit def column[C <: Column[_]]: GroupByColumns[C, C :: HNil] = new GroupByColumns[C, C :: HNil] {
    override def apply(fields: C): C :: HNil = fields :: HNil
  }

  implicit def aggregation: GroupByColumns[Aggregation[_, _], HNil] = new GroupByColumns[Aggregation[_, _], HNil] {
    override def apply(fields: Aggregation[_, _]): HNil = HNil
  }

  implicit def cast[F <: Field[_], C <: HList](
    implicit
    extractColumns: GroupByColumns[F, C]
  ): GroupByColumns[Cast[F, _], C] = new GroupByColumns[Cast[F, _], C] {
    override def apply(fields: Cast[F, _]): C = extractColumns(fields.field)
  }

  implicit def softCast[F <: Field[_], C <: HList](
    implicit
    extractColumns: GroupByColumns[F, C]
  ): GroupByColumns[SoftCast[F, _], C] = new GroupByColumns[SoftCast[F, _], C] {
    override def apply(fields: SoftCast[F, _]): C = extractColumns(fields.field)
  }

  implicit def fieldExpr1[F <: Field[_], C <: HList](
    implicit
    extractColumns: GroupByColumns[F, C]
  ): GroupByColumns[FieldExpr1[F, _], C] = new GroupByColumns[FieldExpr1[F, _], C] {
    override def apply(fields: FieldExpr1[F, _]): C = extractColumns(fields.field)
  }

  implicit def fieldExpr2[F1 <: Field[_], F2 <: Field[_], C1 <: HList, C2 <: HList, C <: HList](
    implicit
    extractColumns1: GroupByColumns[F1, C1],
    extractColumns2: GroupByColumns[F2, C2],
    concat: Concat.Aux[C1, C2, C]
  ): GroupByColumns[FieldExpr2[F1, F2, _], C] = new GroupByColumns[FieldExpr2[F1, F2, _], C] {
    override def apply(fields: FieldExpr2[F1, F2, _]): C = concat(extractColumns1(fields.field1), extractColumns2(fields.field2))
  }

  implicit val hNil: GroupByColumns[HNil, HNil] = new GroupByColumns[HNil, HNil] {
    override def apply(fields: HNil): HNil = HNil
  }

  implicit def hCons[FHead, CHead <: HList, FTail <: HList, CTail <: HList, C <: HList](
    implicit
    extractHead: GroupByColumns[FHead, CHead],
    extractTail: GroupByColumns[FTail, CTail],
    concat: Concat.Aux[CHead, CTail, C]
  ): GroupByColumns[FHead :: FTail, C] = new GroupByColumns[FHead :: FTail, C] {
    override def apply(fields: FHead :: FTail): C =
      concat(extractHead(fields.head), extractTail(fields.tail))
  }
}

@implicitNotFound("Could not build a schema for this table.\n" +
  " Possible reasons:\n" +
  " - your schema is invalid: ensure that all properties in your schema are of type Column[T] with Tag[A]" +
  " for concrete T and A;\n" +
  " - a FieldEncoder could not be found for one or more columns in your schema." +
  " Ensure that a FieldEncoder[T] is in scope for every Column[T] in your schema.")
trait Columns[+X] {
  def apply(tableName: String): X
}

object Columns {
  implicit def pure[Type, Name <: String with Singleton](
    implicit
    decoder: FieldDecoder[Type],
    name: ValueOf[Name]
  ): Columns[Column[Type] with Tagging[Name]] =
    (tableName: String) => new Column[Type](name.value, tableName) with Tagging[Name] {
      override def tagValue: String = name
    }

  implicit val hNil: Columns[HNil] =
    (_: String) => HNil

  implicit def hCons[H, T <: HList](
    implicit
    headColumn: Columns[H],
    tailColumns: Columns[T]
  ): Columns[H :: T] =
    (tableName: String) => headColumn(tableName) :: tailColumns(tableName)
}

@implicitNotFound("Could not build a schema for ${Schema}.\n" +
  " Possible reason: a FieldEncoder could not be found for one or more columns in your schema." +
  " Ensure that a FieldEncoder[T] is in scope for every T in your schema.")
trait GenericSchema[Schema] {
  type Repr
}

object GenericSchema {
  type Aux[Schema, T] = GenericSchema[Schema] { type Repr = T }

  implicit def labelled[T, A <: String with Singleton](
    implicit
    fieldDecoder: FieldDecoder[T],
    alias: ValueOf[A]
  ): GenericSchema.Aux[FieldType[Symbol @@ A, T], Column[T] with Tagging[A]] =
    new GenericSchema[FieldType[Symbol @@ A, T]] {
      override type Repr = Column[T] with Tagging[A]
    }

  implicit def pure[T, A <: String with Singleton](
    implicit
    fieldDecoder: FieldDecoder[T],
    alias: ValueOf[A]
  ): GenericSchema.Aux[Column[T] with Tagging[A], Column[T] with Tagging[A]] =
    new GenericSchema[Column[T] with Tagging[A]] {
      override type Repr = Column[T] with Tagging[A]
    }

  implicit def hNil: GenericSchema.Aux[HNil, HNil] = new GenericSchema[HNil] {
    override type Repr = HNil
  }

  implicit def hCons[Head, HeadRepr, Tail <: HList, TailRepr <: HList](
    implicit
    headBuilder: GenericSchema.Aux[Head, HeadRepr],
    tailBuilder: GenericSchema.Aux[Tail, TailRepr]
  ): GenericSchema.Aux[Head :: Tail, HeadRepr :: TailRepr] = new GenericSchema[Head :: Tail] {
    override type Repr = HeadRepr :: TailRepr
  }

//  implicit def serializableProduct[P <: Product with Serializable, IntermediateRepr <: HList, FinalRepr <: HList](
//    implicit
//    generic: LabelledGeneric.Aux[P, IntermediateRepr],
//    schemaBuilder: GenericSchema.Aux[IntermediateRepr, FinalRepr]
//  ): GenericSchema.Aux[P, FinalRepr] = new GenericSchema[P] {
//    override type Repr = FinalRepr
//  }

  // This will become legacy as soon as the following gets fixed
  // https://youtrack.jetbrains.com/issue/SCL-16639
  implicit def hackForIntellij[P <: Product, FinalRepr <: HList](
    implicit
    tupler: Tupler.Aux[FinalRepr, P],
    columns: Columns[FinalRepr]
  ): GenericSchema.Aux[P, FinalRepr] = new GenericSchema[P] {
    override type Repr = FinalRepr
  }
}

@implicitNotFound("The required placeholders for this query: ${Values} do not match with type ${T}.")
trait PlaceholderValues[-T, +Values] {
  def apply(v: T): Values
}

object PlaceholderValues {
  implicit def head[T, A <: String with Singleton](
    implicit
    alias: ValueOf[A],
    fieldEncoder: FieldEncoder[T],
    fieldDecoder: FieldDecoder[T]
  ): PlaceholderValues[FieldType[Symbol @@ A, T], Literal[T] with Tagging[A]] =
    (v: FieldType[Symbol @@ A, T]) => Literal(v.asInstanceOf[T]) as alias.value

  implicit def hNil: PlaceholderValues[HNil, HNil] =
    (_: HNil) => HNil

  implicit def hCons[Head, HeadValuesRepr, Tail <: HList, TailValuesRepr <: HList](
    implicit
    headBuilder: PlaceholderValues[Head, HeadValuesRepr],
    tailBuilder: PlaceholderValues[Tail, TailValuesRepr]
  ): PlaceholderValues[Head :: Tail, HeadValuesRepr :: TailValuesRepr] =
    (v: Head :: Tail) => headBuilder(v.head) :: tailBuilder(v.tail)

  implicit def fromCaseClass[C, R1, Values](
    implicit
    labelledGeneric: LabelledGeneric.Aux[C, R1],
    schemaBuilder: PlaceholderValues[R1, Values]
  ): PlaceholderValues[C, Values] =
    (v: C) => schemaBuilder(labelledGeneric.to(v))
}
