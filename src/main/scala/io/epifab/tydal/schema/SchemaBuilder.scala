package io.epifab.tydal.schema

import io.epifab.tydal.Tagging
import shapeless.labelled.FieldType
import shapeless.tag.@@
import shapeless.{::, HList, HNil, LabelledGeneric}

import scala.annotation.implicitNotFound

@implicitNotFound("Could not build a schema from ${T}.\n" +
  " Possible reason: a FieldEncoder could not be found for one or more columns in your schema." +
  " Ensure that a FieldEncoder[T] is in scope for every T in your schema.")
trait SchemaBuilder[-T, +Repr] {
  def apply(relationName: String): Repr
}

object SchemaBuilder {
  implicit def labelled[T, A <: String with Singleton](
    implicit
    fieldDecoder: FieldDecoder[T],
    alias: ValueOf[A]
  ): SchemaBuilder[FieldType[Symbol @@ A, T], Column[T] with Tagging[A]] =
    (relationAlias: String) =>
      new Column[T](alias.value, relationAlias) with Tagging[A] {
        override def tagValue: String = alias.value
      }

  implicit def hNil: SchemaBuilder[HNil, HNil] =
    (_: String) => HNil

  implicit def hCons[Head, HeadRepr, Tail <: HList, TailRepr <: HList](
    implicit
    headBuilder: SchemaBuilder[Head, HeadRepr],
    tailBuilder: SchemaBuilder[Tail, TailRepr]
  ): SchemaBuilder[Head :: Tail, HeadRepr :: TailRepr] =
    (relationAlias: String) => headBuilder(relationAlias) :: tailBuilder(relationAlias)

  implicit def fromCaseClass[CaseClass, IntermediateRepr, Repr](
    implicit
    generic: LabelledGeneric.Aux[CaseClass, IntermediateRepr],
    schemaBuilder: SchemaBuilder[IntermediateRepr, Repr]
  ): SchemaBuilder[CaseClass, Repr] =
    (relationName: String) => schemaBuilder(relationName)
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
