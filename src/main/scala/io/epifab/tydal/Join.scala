package io.epifab.tydal

import io.epifab.tydal.fields.{Column, Field, NullableField}
import shapeless.{::, HList, HNil}

trait LeftJoinFields[-FIELDS, +JOIN_FIELDS] {
  def build(fields: FIELDS): JOIN_FIELDS
}

object LeftJoinFields {
  implicit def singleField[F <: Field[_], G <: Field[_]]
      (implicit
       nullableField: NullableField[F, G]): LeftJoinFields[F, G] =
    (field: F) => nullableField(field)

  implicit def hNil: SubQueryFields[HNil, HNil] =
    (_: String, _: HNil) => HNil

  implicit def hCons[H, RH, T <: HList, RT <: HList](implicit headField: SubQueryFields[H, RH], tailFields: SubQueryFields[T, RT]): SubQueryFields[H :: T, RH :: RT] =
    (srcAlias: String, list: H :: T) => headField.build(srcAlias, list.head) :: tailFields.build(srcAlias, list.tail)
}
