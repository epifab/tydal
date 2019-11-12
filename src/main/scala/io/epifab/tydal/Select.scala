package io.epifab.tydal

import io.epifab.tydal.fields._
import io.epifab.tydal.runner.{QueryBuilder, ReadStatementStep1, StatementBuilder}
import io.epifab.tydal.utils.{Appender, Bounded, TaggedFinder}
import shapeless.ops.hlist.Tupler
import shapeless.{::, Generic, HList, HNil}

class Join[+S <: Selectable[_] with Tag[_], +E <: BinaryExpr](val selectable: S, val filter: E)

sealed trait SortOrder

case class SortBy[+F <: Field[_] with Tag[_]](field: F, sortOrder: SortOrder) {
  def alias: String = field.tagValue
}

object Ascending {
  case object AscendingOrder extends SortOrder

  def apply[F <: Field[_] with Tag[_]](field: F): SortBy[F] =
    SortBy(field, AscendingOrder)
}

object Descending {
  case object DescendingOrder extends SortOrder

  def apply[F <: Field[_] with Tag[_]](field: F): SortBy[F] =
    SortBy(field, DescendingOrder)
}

trait SelectContext[FIELDS <: HList, SOURCES <: HList] extends FindContext[(FIELDS, SOURCES)] {
  def $fields: FIELDS
  def $sources: SOURCES

  override def apply[TAG <: String with Singleton, X](tag: TAG)(implicit finder: TaggedFinder[TAG, X, (FIELDS, SOURCES)]): X with Tag[TAG] =
    finder.find(($fields, $sources))

  def apply[TAG1 <: String with Singleton, TAG2 <: String with Singleton, X2, HAYSTACK2]
      (tag1: TAG1, tag2: TAG2)
      (implicit
       finder1: TaggedFinder[TAG1, FindContext[HAYSTACK2], SOURCES],
       finder2: TaggedFinder[TAG2, X2, HAYSTACK2]): X2 AS TAG2 =
    finder1.find($sources).apply[TAG2, X2](tag2)
}

sealed trait Select[FIELDS <: HList, GROUP_BY <: HList, SOURCES <: HList, WHERE <: BinaryExpr, HAVING <: BinaryExpr, SORT_BY <: HList] extends SelectContext[FIELDS, SOURCES] { select =>
  def $fields: FIELDS
  def $groupBy: GROUP_BY
  def $sources: SOURCES
  def $where: WHERE
  def $having: HAVING
  def $sortBy: SORT_BY

  def as[TAG <: String with Singleton, SUBQUERY_FIELDS <: HList](tag: TAG)(implicit subQueryFields: SubQueryFields[FIELDS, SUBQUERY_FIELDS]): SelectSubQuery[SUBQUERY_FIELDS, Select[FIELDS, GROUP_BY, SOURCES, WHERE, HAVING, SORT_BY]] with Tag[TAG] =
    new SelectSubQuery(select, subQueryFields.build(tag, select.$fields)) with Tag[TAG] {
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

  def from[T <: Selectable[_] with Tag[_]](source: T)(implicit queryBuilder: QueryBuilder[Select[HNil, HNil, T :: HNil, AlwaysTrue, AlwaysTrue, HNil], HNil, HNil]): NonEmptySelect[HNil, HNil, T :: HNil, AlwaysTrue, AlwaysTrue, HNil] =
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
    override def apply[TAG <: String with Singleton, X](tag: TAG)(implicit finder: TaggedFinder[TAG, X, FIELDS]): X with Tag[TAG] =
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

  def take1[F <: Field[_] with Tag[_]]
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

  def groupBy1[F <: Field[_] with Tag[_]]
    (f: SelectContext[FIELDS, SOURCES] => F)
    (implicit
     queryBuilder: QueryBuilder[Select[FIELDS, F :: HNil, SOURCES, WHERE, HAVING, SORT_BY], _, FIELDS]):
    NonEmptySelect[FIELDS, F :: HNil, SOURCES, WHERE, HAVING, SORT_BY] =
      new NonEmptySelect($fields, f(this) :: HNil, $sources, $where, $having, $sortBy)

  def innerJoin[NEW_SOURCE <: Selectable[_] with Tag[_]](that: NEW_SOURCE): JoinBuilder[FIELDS, GROUP_BY, SOURCES, WHERE, HAVING, SORT_BY, NEW_SOURCE] =
    new JoinBuilder(this, that)

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

class JoinBuilder[
  FIELDS <: HList,
  GROUP_BY <: HList,
  SOURCES <: HList,
  WHERE <: BinaryExpr,
  HAVING <: BinaryExpr,
  SORT_BY <: HList,
  NEW_SOURCE <: Selectable[_] with Tag[_]](select: Select[FIELDS, GROUP_BY, SOURCES, WHERE, HAVING, SORT_BY], that: NEW_SOURCE) {
  def on[JOIN_CLAUSE <: BinaryExpr, SOURCE_RESULTS <: HList]
      (f: (NEW_SOURCE, SelectContext[FIELDS, SOURCES]) => JOIN_CLAUSE)
      (implicit
       appender: Appender.Aux[SOURCES, Join[NEW_SOURCE, JOIN_CLAUSE], SOURCE_RESULTS],
       queryBuilder: QueryBuilder[Select[FIELDS, GROUP_BY, SOURCE_RESULTS, WHERE, HAVING, SORT_BY], _, FIELDS]): NonEmptySelect[FIELDS, GROUP_BY, SOURCE_RESULTS, WHERE, HAVING, SORT_BY] =
    new NonEmptySelect(
      select.$fields,
      select.$groupBy,
      appender.append(select.$sources, new Join(that, f(that, select))),
      select.$where,
      select.$having,
      select.$sortBy
    )
}
