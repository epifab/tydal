package io.epifab.tydal

import io.epifab.tydal.fields.{BinaryExpr, Field, NullableField}
import io.epifab.tydal.runner.QueryBuilder
import io.epifab.tydal.utils.{Appender, TaggedFinder}
import shapeless.{::, HList, HNil}

class JoinBuilder[
    Fields <: HList,
    GroupBy <: HList,
    Sources <: HList,
    Where <: BinaryExpr,
    Having <: BinaryExpr,
    Sort <: HList,
    RightSource <: Selectable[_] with Tagging[_],
    RightFields <: HList,
    RightAlias <: String with Singleton
  ](left: Select[Fields, GroupBy, Sources, Where, Having, Sort],
    right: RightSource,
    rightFields: RightFields,
    joinType: JoinType) {

  def on[JoinClause <: BinaryExpr, SourceResults <: HList]
  (f: (RightSource, SelectContext[Fields, Sources]) => JoinClause)
  (implicit
   alias: ValueOf[RightAlias],
   appender: Appender.Aux[Sources, Join[RightSource, RightFields, RightAlias, JoinClause], SourceResults],
   queryBuilder: QueryBuilder[Select[Fields, GroupBy, SourceResults, Where, Having, Sort], _, Fields]): NonEmptySelect[Fields, GroupBy, SourceResults, Where, Having, Sort] =
    new NonEmptySelect(
      left.$fields,
      left.$groupBy,
      appender.append(
        left.$sources,
        new Join(right, rightFields, f(right, left), joinType)
      ),
      left.$where,
      left.$having,
      left.$sortBy
    )
}

trait NullableFields[-From, +To] {
  def build(fields: From): To
}

object NullableFields {
  implicit def singleField[F <: Field[_], G <: Field[_]]
      (implicit nullableField: NullableField[F, G]): NullableFields[F, G] =
    (field: F) => nullableField(field)

  implicit def hNil: NullableFields[HNil, HNil] =
    (_: HNil) => HNil

  implicit def hCons[H, RH, T <: HList, RT <: HList](implicit headField: NullableFields[H, RH], tailFields: NullableFields[T, RT]): NullableFields[H :: T, RH :: RT] =
    (list: H :: T) => headField.build(list.head) :: tailFields.build(list.tail)
}

class Join[Right <: Selectable[_] with Tagging[_], RightFields <: HList, RightAlias <: String with Singleton, JoinClause <: BinaryExpr]
  (val right: Right, override val fields: RightFields, val joinClause: JoinClause, val joinType: JoinType)
  (implicit alias: ValueOf[RightAlias]) extends Selectable[RightFields] with Tagging[RightAlias] {

  override def apply[T <: String with Singleton, X](tag: T)(implicit finder: TaggedFinder[T, X, RightFields]): X with Tagging[T] =
    finder.find(fields)

  override def tagValue: String = alias.value
}

sealed trait JoinType
case object InnerJoin extends JoinType
case object LeftJoin extends JoinType
