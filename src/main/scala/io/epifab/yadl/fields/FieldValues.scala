package io.epifab.yadl.fields

import io.epifab.yadl.{Tag, Tagged}
import shapeless.{::, Generic, HList, HNil}

trait FieldValues[FIELDS, VALUES] { }

object FieldValues {
  implicit def field[F <: Field[_] with Tag[_], T, A <: String]
      (implicit
       fieldT: FieldT[F, T],
       taggedField: Tagged[F, A]): FieldValues[F, PlaceholderValue[T] with Tag[A]] =
    new FieldValues[F, PlaceholderValue[T] with Tag[A]] { }

  implicit val hNil: FieldValues[HNil, HNil] = new FieldValues[HNil, HNil] {}

  implicit def hCons[H, HX, T <: HList, TX <: HList]
      (implicit
       head: FieldValues[H, HX],
       tail: FieldValues[T, TX]): FieldValues[H :: T, HX :: TX] =
    new FieldValues[H :: T, HX :: TX] {}

  implicit def caseClass[FIELDS <: HList, VALUES <: HList, CC]
      (implicit
       generic: Generic.Aux[CC, VALUES],
       values: FieldValues[FIELDS, VALUES]): FieldValues[FIELDS, CC] =
    new FieldValues[FIELDS, CC] {}
}
