package io.epifab.tydal

import io.epifab.tydal.fields.{Column, FieldDecoder, FieldEncoder, PlaceholderValue}
import shapeless.{::, HList, HNil, LabelledGeneric, labelled, tag}

import scala.annotation.implicitNotFound

@implicitNotFound("Could not build a schema for this table.\n" +
  " Possible reason: a FieldEncoder could not be found for one or more columns in your schema." +
  " Ensure that a FieldEncoder[T] is in scope for every T in your schema.")
trait SchemaBuilder[T, +Repr] {
  def build(relationName: String): Repr
}

object SchemaBuilder {
  implicit def head[T, A <: String with Singleton](implicit alias: ValueOf[A], fieldDecoder: FieldDecoder[T]): SchemaBuilder[T with labelled.KeyTag[Symbol with tag.Tagged[A], T], Column[T] with Tagging[A]] =
    (relationAlias: String) =>
      new Column[T](alias.value, relationAlias) with Tagging[A] {
        override def tagValue: String = alias.value
      }

  implicit def hNil: SchemaBuilder[HNil, HNil] =
    (_: String) => HNil

  implicit def hCons[Head, HeadRepr, Tail <: HList, TailRepr <: HList](implicit head: SchemaBuilder[Head, HeadRepr], tail: SchemaBuilder[Tail, TailRepr]): SchemaBuilder[Head :: Tail, HeadRepr :: TailRepr] =
    (relationAlias: String) => head.build(relationAlias) :: tail.build(relationAlias)

  implicit def fromCaseClass[C, R1, Schema](implicit labelledGeneric: LabelledGeneric.Aux[C, R1], schemaBuilder: SchemaBuilder[R1, Schema]): SchemaBuilder[C, Schema] =
    (relationName: String) => schemaBuilder.build(relationName)
}

@implicitNotFound("The required placeholders for this query: ${Values} do not match with type ${T}.")
trait PlaceholderValuesBuilder[-T, +Values] {
  def values(v: T): Values
}

object PlaceholderValuesBuilder {
  implicit def head[T, A <: String with Singleton]
  (implicit
   alias: ValueOf[A],
   fieldEncoder: FieldEncoder[T],
   fieldDecoder: FieldDecoder[T]
  ): PlaceholderValuesBuilder[T with labelled.KeyTag[Symbol with tag.Tagged[A], T], PlaceholderValue[T] with Tagging[A]] =
    (v: T with labelled.KeyTag[Symbol with tag.Tagged[A], T]) => PlaceholderValue(v.asInstanceOf[T]) as alias.value

  implicit def hNil: PlaceholderValuesBuilder[HNil, HNil] =
    (_: HNil) => HNil

  implicit def hCons[Head, HeadValuesRepr, Tail <: HList, TailValuesRepr <: HList]
  (implicit
   head: PlaceholderValuesBuilder[Head, HeadValuesRepr],
   tail: PlaceholderValuesBuilder[Tail, TailValuesRepr]): PlaceholderValuesBuilder[Head :: Tail, HeadValuesRepr :: TailValuesRepr] =
    (v: Head :: Tail) => head.values(v.head) :: tail.values(v.tail)

  implicit def fromCaseClass[C, R1, Values](implicit labelledGeneric: LabelledGeneric.Aux[C, R1], schemaBuilder: PlaceholderValuesBuilder[R1, Values]): PlaceholderValuesBuilder[C, Values] =
    (v: C) => schemaBuilder.values(labelledGeneric.to(v))
}
