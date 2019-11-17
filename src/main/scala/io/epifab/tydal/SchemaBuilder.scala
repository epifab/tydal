package io.epifab.tydal

import io.epifab.tydal.fields.{Column, FieldDecoder}
import shapeless.{::, HList, HNil, LabelledGeneric, labelled, tag}

import scala.annotation.implicitNotFound

@implicitNotFound("Could not build a schema for this table.\n" +
  " Possible reason: a FieldEncoder could not be found for one or more columns in your schema." +
  " Ensure that a FieldEncoder[T] is in scope for every T in your schema.")
trait SchemaBuilder[T, +REPR] {
  def build(relationName: String): REPR
}

object SchemaBuilder {
  implicit def head[T, A <: Tag](implicit alias: ValueOf[A], fieldDecoder: FieldDecoder[T]): SchemaBuilder[T with labelled.KeyTag[Symbol with tag.Tagged[A], T], Column[T] with Tagging[A]] =
    (relationAlias: String) =>
      new Column[T](alias.value, relationAlias) with Tagging[A] {
        override def tagValue: String = alias.value
      }

  implicit def hNil: SchemaBuilder[HNil, HNil] =
    (_: String) => HNil

  implicit def hCons[HEAD, HEAD_REPR, TAIL <: HList, TAIL_REPR <: HList](implicit head: SchemaBuilder[HEAD, HEAD_REPR], tail: SchemaBuilder[TAIL, TAIL_REPR]): SchemaBuilder[HEAD :: TAIL, HEAD_REPR :: TAIL_REPR] =
    (relationAlias: String) => head.build(relationAlias) :: tail.build(relationAlias)

  implicit def fromCaseClass[C, R1, SCHEMA](implicit labelledGeneric: LabelledGeneric.Aux[C, R1], schemaBuilder: SchemaBuilder[R1, SCHEMA]): SchemaBuilder[C, SCHEMA] =
    (relationName: String) => schemaBuilder.build(relationName)
}
