package io.epifab.tydal.queries

import io.epifab.tydal._
import io.epifab.tydal.schema.{Filter, Field, NullableField}
import io.epifab.tydal.utils.{Appender, TaggedFinder}
import shapeless.ops.hlist.Tupler
import shapeless.{::, HList, HNil}

class JoinBuilder[
    Fields <: HList,
    GroupBy <: HList,
    Sources <: HList,
    Where <: Filter,
    Having <: Filter,
    Sort <: HList,
    RightSource <: Selectable[_] with Tagging[_],
    RightFields <: HList,
    RightAlias <: String with Singleton
  ](left: SelectQuery[Fields, GroupBy, Sources, Where, Having, Sort],
    right: RightSource,
    rightFields: RightFields,
    joinType: JoinType) {

  def on[JoinClause <: Filter, SourceResults <: HList]
  (f: (RightSource, SelectContext[Fields, Sources]) => JoinClause)
  (implicit
   alias: ValueOf[RightAlias],
   appender: Appender.Aux[Sources, Join[RightSource, RightFields, JoinClause] As RightAlias, SourceResults],
   queryBuilder: QueryBuilder[SelectQuery[Fields, GroupBy, SourceResults, Where, Having, Sort], _, Fields]): SelectQuery[Fields, GroupBy, SourceResults, Where, Having, Sort] =
    new SelectQuery(
      left.fields,
      left.groupBy,
      appender.append(
        left.sources,
        new Join(right, rightFields, f(right, left), joinType) with Tagging[RightAlias] {
          override def tagValue: String = alias.value
        }
      ),
      left.where,
      left.having,
      left.sortBy
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

class Join[Right <: Selectable[_] with Tagging[_], RightFields <: HList, JoinClause <: Filter](
  val right: Right,
  override val fields: RightFields,
  val joinClause: JoinClause,
  val joinType: JoinType
) extends Selectable[RightFields] { Self: Tagging[_] =>

  override def apply[T <: String with Singleton, X](tag: T)(implicit finder: TaggedFinder[T, X, RightFields]): X with Tagging[T] =
    finder.find(fields)

  def `*`[T](implicit tupler: Tupler.Aux[RightFields, T]): T = tupler(fields)

}

sealed trait JoinType
case object InnerJoin extends JoinType
case object LeftJoin extends JoinType
