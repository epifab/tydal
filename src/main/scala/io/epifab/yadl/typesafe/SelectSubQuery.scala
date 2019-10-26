package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields.{Column, Field}
import io.epifab.yadl.typesafe.utils.TaggedFinder
import shapeless.{::, HList, HNil}

class SelectSubQuery[SUBQUERY_FIELDS <: HList, S <: Select[_, _, _, _]]
  (val select: S, override val fields: SUBQUERY_FIELDS)
  extends Selectable[SUBQUERY_FIELDS] { self: Tag[_] =>
  override def apply[TAG <: String with Singleton, X](tag: TAG)(implicit finder: TaggedFinder[TAG, X, SUBQUERY_FIELDS]): X with Tag[TAG] =
    finder.find(fields)
}

trait SubQueryFields[-FIELDS, +SUBQUERY_FIELDS] {
  def build(srcAlias: String, fields: FIELDS): SUBQUERY_FIELDS
}

object SubQueryFields {
  implicit def singleField[T, ALIAS <: String]: SubQueryFields[Field[T] with Tag[ALIAS], Column[T] with Tag[ALIAS]] =
    (srcAlias: String, field: Field[T] with Tag[ALIAS]) => new Column(field.tagValue, srcAlias)(field.decoder) with Tag[ALIAS] {
      override def tagValue: String = field.tagValue
    }

  implicit def hNil: SubQueryFields[HNil, HNil] =
    (_: String, _: HNil) => HNil

  implicit def hCons[H, RH, T <: HList, RT <: HList](implicit headField: SubQueryFields[H, RH], tailFields: SubQueryFields[T, RT]): SubQueryFields[H :: T, RH :: RT] =
    (srcAlias: String, list: H :: T) => headField.build(srcAlias, list.head) :: tailFields.build(srcAlias, list.tail)
}