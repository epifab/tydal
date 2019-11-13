package io.epifab.tydal

import io.epifab.tydal.fields.{Column, Field}
import io.epifab.tydal.utils.TaggedFinder
import shapeless.{::, HList, HNil}

class SelectSubQuery[SUBQUERY_FIELDS <: HList, S <: Select[_, _, _, _, _, _]]
  (val select: S, override val schema: SUBQUERY_FIELDS)
  extends Selectable[SUBQUERY_FIELDS] { self: Tagging[_] =>
  override def apply[T <: Tag, X](tag: T)(implicit finder: TaggedFinder[T, X, SUBQUERY_FIELDS]): X with Tagging[T] =
    finder.find(schema)
}

trait SubQueryFields[-FIELDS, +SUBQUERY_FIELDS] {
  def build(srcAlias: String, fields: FIELDS): SUBQUERY_FIELDS
}

object SubQueryFields {
  implicit def singleField[T, ALIAS <: Tag]: SubQueryFields[Field[T] with Tagging[ALIAS], Column[T] with Tagging[ALIAS]] =
    (srcAlias: String, field: Field[T] with Tagging[ALIAS]) => new Column(field.tagValue, srcAlias)(field.decoder) with Tagging[ALIAS] {
      override def tagValue: String = field.tagValue
    }

  implicit def hNil: SubQueryFields[HNil, HNil] =
    (_: String, _: HNil) => HNil

  implicit def hCons[H, RH, T <: HList, RT <: HList](implicit headField: SubQueryFields[H, RH], tailFields: SubQueryFields[T, RT]): SubQueryFields[H :: T, RH :: RT] =
    (srcAlias: String, list: H :: T) => headField.build(srcAlias, list.head) :: tailFields.build(srcAlias, list.tail)
}
