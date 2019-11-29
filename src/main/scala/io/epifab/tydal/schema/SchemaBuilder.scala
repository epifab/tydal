package io.epifab.tydal.schema

import io.epifab.tydal.Tagging
import shapeless.labelled.FieldType
import shapeless.ops.hlist.Tupler
import shapeless.tag.@@
import shapeless.{::, HList, HNil, LabelledGeneric}

import scala.annotation.implicitNotFound

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

//  implicit def labelled[T, A <: String with Singleton](
//    implicit
//    fieldDecoder: FieldDecoder[T],
//    alias: ValueOf[A]
//  ): GenericSchema.Aux[FieldType[Symbol @@ A, T], Column[T] with Tagging[A]] =
//    new GenericSchema[FieldType[Symbol @@ A, T]] {
//      override type Repr = Column[T] with Tagging[A]
//    }
//
//  implicit def hNil: GenericSchema.Aux[HNil, HNil] = new GenericSchema[HNil] {
//    override type Repr = HNil
//  }
//
//  implicit def hCons[Head, HeadRepr, Tail <: HList, TailRepr <: HList](
//    implicit
//    headBuilder: GenericSchema.Aux[Head, HeadRepr],
//    tailBuilder: GenericSchema.Aux[Tail, TailRepr]
//  ): GenericSchema.Aux[Head :: Tail, HeadRepr :: TailRepr] = new GenericSchema[Head :: Tail] {
//    override type Repr = HeadRepr :: TailRepr
//  }
//
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
  ): PlaceholderValues[FieldType[Symbol @@ A, T], PlaceholderValue[T] with Tagging[A]] =
    (v: FieldType[Symbol @@ A, T]) => PlaceholderValue(v.asInstanceOf[T]) as alias.value

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
