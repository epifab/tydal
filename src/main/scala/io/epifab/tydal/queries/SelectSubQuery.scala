package io.epifab.tydal.queries

import io.epifab.tydal.Tagging
import io.epifab.tydal.schema.{Column, Field}
import io.epifab.tydal.utils.TaggedFind
import shapeless.{::, HList, HNil}

class SelectSubQuery[SubQueryFields <: HList, S <: SelectQuery[_, _, _, _, _, _]]
  (val select: S, override val fields: SubQueryFields)
  extends Selectable[SubQueryFields] { self: Tagging[_] =>
  override def apply[T <: String with Singleton, X](tag: T)(implicit find: TaggedFind[T, X, SubQueryFields]): X with Tagging[T] =
    find(fields)
}

trait SubQueryFields[-Fields, +SubQueryFields] {
  def build(srcAlias: String, fields: Fields): SubQueryFields
}

object SubQueryFields {
  implicit def singleField[T, Alias <: String with Singleton]: SubQueryFields[Field[T] with Tagging[Alias], Column[T] with Tagging[Alias]] =
    (srcAlias: String, field: Field[T] with Tagging[Alias]) => new Column(field.tagValue, srcAlias)(field.decoder) with Tagging[Alias] {
      override def tagValue: String = field.tagValue
    }

  implicit def hNil: SubQueryFields[HNil, HNil] =
    (_: String, _: HNil) => HNil

  implicit def hCons[H, RH, T <: HList, RT <: HList](implicit headField: SubQueryFields[H, RH], tailFields: SubQueryFields[T, RT]): SubQueryFields[H :: T, RH :: RT] =
    (srcAlias: String, list: H :: T) => headField.build(srcAlias, list.head) :: tailFields.build(srcAlias, list.tail)
}
