package io.epifab.tydal

import io.epifab.tydal.fields._
import io.epifab.tydal.runner.{QueryBuilder, ReadStatementStep1, StatementBuilder}
import io.epifab.tydal.utils.{Appender, Bounded, TaggedFinder}
import shapeless.ops.hlist.Tupler
import shapeless.{::, Generic, HList, HNil}

sealed trait SortOrder

case class SortBy[+F <: Field[_] with Tagging[_]](field: F, sortOrder: SortOrder) {
  def alias: String = field.tagValue
}

object Ascending {
  case object AscendingOrder extends SortOrder

  def apply[F <: Field[_] with Tagging[_]](field: F): SortBy[F] =
    SortBy(field, AscendingOrder)
}

object Descending {
  case object DescendingOrder extends SortOrder

  def apply[F <: Field[_] with Tagging[_]](field: F): SortBy[F] =
    SortBy(field, DescendingOrder)
}

trait SelectContext[FIELDS <: HList, SOURCES <: HList] extends FindContext[(FIELDS, SOURCES)] {
  def $fields: FIELDS
  def $sources: SOURCES

  override def apply[T <: Tag, X](tag: T)(implicit finder: TaggedFinder[T, X, (FIELDS, SOURCES)]): X with Tagging[T] =
    finder.find(($fields, $sources))

  def apply[T1 <: Tag, T2 <: Tag, X2, HAYSTACK2]
      (tag1: T1, tag2: T2)
      (implicit
       finder1: TaggedFinder[T1, FindContext[HAYSTACK2], SOURCES],
       finder2: TaggedFinder[T2, X2, HAYSTACK2]): X2 AS T2 =
    finder1.find($sources).apply[T2, X2](tag2)
}

sealed trait Select[FIELDS <: HList, GROUP_BY <: HList, SOURCES <: HList, WHERE <: BinaryExpr, HAVING <: BinaryExpr, SORT_BY <: HList] extends SelectContext[FIELDS, SOURCES] { select =>
  def $fields: FIELDS
  def $groupBy: GROUP_BY
  def $sources: SOURCES
  def $where: WHERE
  def $having: HAVING
  def $sortBy: SORT_BY

  def as[T <: Tag with Singleton, SUBQUERY_FIELDS <: HList](tag: T)(implicit subQueryFields: SubQueryFields[FIELDS, SUBQUERY_FIELDS]): SelectSubQuery[SUBQUERY_FIELDS, Select[FIELDS, GROUP_BY, SOURCES, WHERE, HAVING, SORT_BY]] with Tagging[T] =
    new SelectSubQuery(select, subQueryFields.build(tag, select.$fields)) with Tagging[T] {
      override def tagValue: String = tag
    }

  def compile[PLACEHOLDERS <: HList, RAW_INPUT <: HList, INPUT]
      (implicit
       queryBuilder: QueryBuilder[this.type, PLACEHOLDERS, FIELDS],
       statementBuilder: StatementBuilder[PLACEHOLDERS, RAW_INPUT, INPUT, FIELDS],
       tupler: Tupler.Aux[RAW_INPUT, INPUT]
      ): ReadStatementStep1[INPUT, FIELDS] =
    statementBuilder.build(queryBuilder.build(this)).select
}

trait EmptySelect extends Select[HNil, HNil, HNil, AlwaysTrue, AlwaysTrue, HNil] {
  override val $fields: HNil = HNil
  override val $groupBy: HNil = HNil
  override val $sources: HNil = HNil
  override val $where: AlwaysTrue = AlwaysTrue
  override val $having: AlwaysTrue = AlwaysTrue
  override val $sortBy: HNil = HNil

  def from[T <: Selectable[_] with Tagging[_]](source: T)(implicit queryBuilder: QueryBuilder[Select[HNil, HNil, T :: HNil, AlwaysTrue, AlwaysTrue, HNil], HNil, HNil]): NonEmptySelect[HNil, HNil, T :: HNil, AlwaysTrue, AlwaysTrue, HNil] =
    new NonEmptySelect($fields, $groupBy, source :: HNil, $where, $having, $sortBy)
}

class NonEmptySelect[FIELDS <: HList, GROUP_BY <: HList, SOURCES <: HList, WHERE <: BinaryExpr, HAVING <: BinaryExpr, SORT_BY <: HList]
    (override val $fields: FIELDS,
     override val $groupBy: GROUP_BY,
     override val $sources: SOURCES,
     override val $where: WHERE,
     override val $having: HAVING,
     override val $sortBy: SORT_BY)
    (implicit queryBuilder: QueryBuilder[Select[FIELDS, GROUP_BY, SOURCES, WHERE, HAVING, SORT_BY], _, FIELDS])
    extends Select[FIELDS, GROUP_BY, SOURCES, WHERE, HAVING, SORT_BY] { Self =>

  private val findContext: FindContext[FIELDS] = new FindContext[FIELDS] {
    override def apply[T <: Tag, X](tag: T)(implicit finder: TaggedFinder[T, X, FIELDS]): X with Tagging[T] =
      finder.find($fields)
  }

  def take[P, NEW_FIELDS <: HList]
    (f: SelectContext[FIELDS, SOURCES] => P)
    (implicit
     generic: Generic.Aux[P, NEW_FIELDS],
     taggedListOfFields: TagMap[Field[_], NEW_FIELDS],
     queryBuilder: QueryBuilder[Select[NEW_FIELDS, GROUP_BY, SOURCES, WHERE, HAVING, SORT_BY], _, NEW_FIELDS]):
    NonEmptySelect[NEW_FIELDS, GROUP_BY, SOURCES, WHERE, HAVING, SORT_BY] =
      new NonEmptySelect(generic.to(f(this)), $groupBy, $sources, $where, $having, $sortBy)

  def take1[F <: Field[_] with Tagging[_]]
    (f: SelectContext[FIELDS, SOURCES] => F)
    (implicit
     queryBuilder: QueryBuilder[Select[F :: HNil, GROUP_BY, SOURCES, WHERE, HAVING, SORT_BY], _, F :: HNil]):
    NonEmptySelect[F :: HNil, GROUP_BY, SOURCES, WHERE, HAVING, SORT_BY] =
      new NonEmptySelect(f(this) :: HNil, $groupBy, $sources, $where, $having, $sortBy)

  def groupBy[P, NEW_GROUPBY <: HList]
    (f: SelectContext[FIELDS, SOURCES] => P)
    (implicit
     generic: Generic.Aux[P, NEW_GROUPBY],
     taggedListOfFields: TagMap[Field[_], NEW_GROUPBY],
     queryBuilder: QueryBuilder[Select[FIELDS, NEW_GROUPBY, SOURCES, WHERE, HAVING, SORT_BY], _, FIELDS]):
    NonEmptySelect[FIELDS, NEW_GROUPBY, SOURCES, WHERE, HAVING, SORT_BY] =
      new NonEmptySelect($fields, generic.to(f(this)), $sources, $where, $having, $sortBy)

  def groupBy1[F <: Field[_] with Tagging[_]]
    (f: SelectContext[FIELDS, SOURCES] => F)
    (implicit
     queryBuilder: QueryBuilder[Select[FIELDS, F :: HNil, SOURCES, WHERE, HAVING, SORT_BY], _, FIELDS]):
    NonEmptySelect[FIELDS, F :: HNil, SOURCES, WHERE, HAVING, SORT_BY] =
      new NonEmptySelect($fields, f(this) :: HNil, $sources, $where, $having, $sortBy)

  def innerJoin[RIGHT_SOURCE <: Selectable[_] with Tagging[_], RIGHT_FIELDS <: HList, RIGHT_ALIAS <: Tag]
    (that: RIGHT_SOURCE)
    (implicit
     selectableFields: SelectableFields[RIGHT_SOURCE, RIGHT_FIELDS],
     tagged: Tagged[RIGHT_SOURCE, RIGHT_ALIAS]): JoinBuilder[FIELDS, GROUP_BY, SOURCES, WHERE, HAVING, SORT_BY, RIGHT_SOURCE, RIGHT_FIELDS, RIGHT_ALIAS] =
    new JoinBuilder(this, that, selectableFields.fields(that), InnerJoin)

//  def leftJoin[NEW_SOURCE <: Selectable[_] with Tagging[_], JOIN_FIELDS <: HList](that: NEW_SOURCE)(implicit fields: NullableFields[]

  def where[NEW_WHERE <: BinaryExpr]
    (f: SelectContext[FIELDS, SOURCES] => NEW_WHERE)
    (implicit queryBuilder: QueryBuilder[Select[FIELDS, GROUP_BY, SOURCES, NEW_WHERE, HAVING, SORT_BY], _, FIELDS]): NonEmptySelect[FIELDS, GROUP_BY, SOURCES, NEW_WHERE, HAVING, SORT_BY] =
    new NonEmptySelect($fields, $groupBy, $sources, f(this), $having, $sortBy)

  def having[NEW_WHERE <: BinaryExpr]
    (f: SelectContext[FIELDS, SOURCES] => NEW_WHERE)
    (implicit queryBuilder: QueryBuilder[Select[FIELDS, GROUP_BY, SOURCES, WHERE, NEW_WHERE, SORT_BY], _, FIELDS]): NonEmptySelect[FIELDS, GROUP_BY, SOURCES, WHERE, NEW_WHERE, SORT_BY] =
    new NonEmptySelect($fields, $groupBy, $sources, $where, f(this), $sortBy)

  def sortBy[P, NEW_SORT_BY <: HList]
    (f: FindContext[FIELDS] => P)
    (implicit
     generic: Generic.Aux[P, NEW_SORT_BY],
     taggedListOfSortByClauses: Bounded[SortBy[_], NEW_SORT_BY],
     queryBuilder: QueryBuilder[Select[FIELDS, GROUP_BY, SOURCES, WHERE, HAVING, NEW_SORT_BY], _, FIELDS]): NonEmptySelect[FIELDS, GROUP_BY, SOURCES, WHERE, HAVING, NEW_SORT_BY] =
    new NonEmptySelect($fields, $groupBy, $sources, $where, $having, generic.to(f(findContext)))
}

object Select extends EmptySelect

