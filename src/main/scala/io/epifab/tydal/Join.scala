package io.epifab.tydal

import io.epifab.tydal.fields.{BinaryExpr, Field, NullableField}
import io.epifab.tydal.runner.QueryBuilder
import io.epifab.tydal.utils.{Appender, TaggedFinder}
import shapeless.{::, HList, HNil}

class JoinBuilder[
    FIELDS <: HList,
    GROUP_BY <: HList,
    SOURCES <: HList,
    WHERE <: BinaryExpr,
    HAVING <: BinaryExpr,
    SORT_BY <: HList,
    RIGHT_SOURCE <: Selectable[_] with Tagging[_],
    RIGHT_FIELDS <: HList,
    RIGHT_ALIAS <: Tag
  ](left: Select[FIELDS, GROUP_BY, SOURCES, WHERE, HAVING, SORT_BY],
    right: RIGHT_SOURCE,
    rightFields: RIGHT_FIELDS,
    joinType: JoinType)
   (implicit tagged: Tagged[RIGHT_SOURCE, RIGHT_ALIAS]) {

  def on[JOIN_CLAUSE <: BinaryExpr, SOURCE_RESULTS <: HList]
  (f: (RIGHT_SOURCE, SelectContext[FIELDS, SOURCES]) => JOIN_CLAUSE)
  (implicit
   appender: Appender.Aux[SOURCES, Join[RIGHT_SOURCE, RIGHT_FIELDS, RIGHT_ALIAS, JOIN_CLAUSE], SOURCE_RESULTS],
   queryBuilder: QueryBuilder[Select[FIELDS, GROUP_BY, SOURCE_RESULTS, WHERE, HAVING, SORT_BY], _, FIELDS]): NonEmptySelect[FIELDS, GROUP_BY, SOURCE_RESULTS, WHERE, HAVING, SORT_BY] =
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

trait NullableFields[-FIELDS, +NULLABLE_FIELDS] {
  def build(fields: FIELDS): NULLABLE_FIELDS
}

object NullableFields {
  implicit def singleField[F <: Field[_], G <: Field[_]]
      (implicit
       nullableField: NullableField[F, G]): NullableFields[F, G] =
    (field: F) => nullableField(field)

  implicit def hNil: NullableFields[HNil, HNil] =
    (_: HNil) => HNil

  implicit def hCons[H, RH, T <: HList, RT <: HList](implicit headField: NullableFields[H, RH], tailFields: NullableFields[T, RT]): NullableFields[H :: T, RH :: RT] =
    (list: H :: T) => headField.build(list.head) :: tailFields.build(list.tail)
}

class Join[SOURCE <: Selectable[_] with Tagging[_], FIELDS <: HList, A <: Tag, E <: BinaryExpr]
  (val right: SOURCE, override val schema: FIELDS, val joinClause: E, val joinType: JoinType)
  (implicit tagged: Tagged[SOURCE, A]) extends Selectable[FIELDS] with Tagging[A] {

  override def apply[T <: Tag, X](tag: T)(implicit finder: TaggedFinder[T, X, FIELDS]): X with Tagging[T] =
    finder.find(schema)

  override def tagValue: String = tagged.tag
}

sealed trait JoinType
case object InnerJoin extends JoinType
case object LeftJoin extends JoinType
