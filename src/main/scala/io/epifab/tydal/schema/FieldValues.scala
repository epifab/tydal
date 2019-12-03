package io.epifab.tydal.schema

import io.epifab.tydal.{Tag, Tagged, Tagging}
import shapeless.{::, Generic, HList, HNil}

trait FieldValues[Fields, Values] { }

object FieldValues {
  implicit def field[F <: Field[_] with Tagging[_], T, A <: String with Singleton]
      (implicit
       fieldT: FieldT[F, T],
       taggedField: Tagged[F, A]): FieldValues[F, Literal[T] with Tagging[A]] =
    new FieldValues[F, Literal[T] with Tagging[A]] { }

  implicit val hNil: FieldValues[HNil, HNil] = new FieldValues[HNil, HNil] {}

  implicit def hCons[H, HX, T <: HList, TX <: HList]
      (implicit
       head: FieldValues[H, HX],
       tail: FieldValues[T, TX]): FieldValues[H :: T, HX :: TX] =
    new FieldValues[H :: T, HX :: TX] {}

  implicit def caseClass[Fields <: HList, Values <: HList, CC]
      (implicit
       generic: Generic.Aux[CC, Values],
       values: FieldValues[Fields, Values]): FieldValues[Fields, CC] =
    new FieldValues[Fields, CC] {}
}
