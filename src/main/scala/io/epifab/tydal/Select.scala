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

trait SelectContext[Fields <: HList, Sources <: HList] extends FindContext[(Fields, Sources)] {
  def $fields: Fields
  def $sources: Sources

  override def apply[T <: String with Singleton, X](tag: T)(implicit finder: TaggedFinder[T, X, (Fields, Sources)]): X with Tagging[T] =
    finder.find(($fields, $sources))

  def apply[T1 <: String with Singleton, T2 <: String with Singleton, X2, HAYSTACK2]
      (tag1: T1, tag2: T2)
      (implicit
       finder1: TaggedFinder[T1, FindContext[HAYSTACK2], Sources],
       finder2: TaggedFinder[T2, X2, HAYSTACK2]): X2 AS T2 =
    finder1.find($sources).apply[T2, X2](tag2)
}

sealed trait Select[Fields <: HList, GroupBy <: HList, Sources <: HList, Where <: BinaryExpr, Having <: BinaryExpr, Sort <: HList] extends SelectContext[Fields, Sources] { select =>
  def $fields: Fields
  def $groupBy: GroupBy
  def $sources: Sources
  def $where: Where
  def $having: Having
  def $sortBy: Sort

  def as[T <: String with Singleton with Singleton, SubQueryFieldsRepr <: HList](tag: T)(implicit subQueryFields: SubQueryFields[Fields, SubQueryFieldsRepr]): SelectSubQuery[SubQueryFieldsRepr, Select[Fields, GroupBy, Sources, Where, Having, Sort]] with Tagging[T] =
    new SelectSubQuery(select, subQueryFields.build(tag, select.$fields)) with Tagging[T] {
      override def tagValue: String = tag
    }

  def compile[Placeholders <: HList, InputRepr <: HList, Input]
      (implicit
       queryBuilder: QueryBuilder[this.type, Placeholders, Fields],
       statementBuilder: StatementBuilder[Placeholders, InputRepr, Input, Fields],
       tupler: Tupler.Aux[InputRepr, Input]
      ): ReadStatementStep1[Input, Fields] =
    statementBuilder.build(queryBuilder.build(this)).select
}

trait EmptySelect extends Select[HNil, HNil, HNil, AlwaysTrue, AlwaysTrue, HNil] {
  override val $fields: HNil = HNil
  override val $groupBy: HNil = HNil
  override val $sources: HNil = HNil
  override val $where: AlwaysTrue = AlwaysTrue
  override val $having: AlwaysTrue = AlwaysTrue
  override val $sortBy: HNil = HNil

  def from[S <: Selectable[_] with Tagging[_]](source: S)(implicit queryBuilder: QueryBuilder[Select[HNil, HNil, S :: HNil, AlwaysTrue, AlwaysTrue, HNil], HNil, HNil]): NonEmptySelect[HNil, HNil, S :: HNil, AlwaysTrue, AlwaysTrue, HNil] =
    new NonEmptySelect($fields, $groupBy, source :: HNil, $where, $having, $sortBy)
}

class NonEmptySelect[Fields <: HList, GroupBy <: HList, Sources <: HList, Where <: BinaryExpr, Having <: BinaryExpr, Sort <: HList]
    (override val $fields: Fields,
     override val $groupBy: GroupBy,
     override val $sources: Sources,
     override val $where: Where,
     override val $having: Having,
     override val $sortBy: Sort)
    (implicit queryBuilder: QueryBuilder[Select[Fields, GroupBy, Sources, Where, Having, Sort], _, Fields])
    extends Select[Fields, GroupBy, Sources, Where, Having, Sort] { Self =>

  private val findContext: FindContext[Fields] = new FindContext[Fields] {
    override def apply[T <: String with Singleton, X](tag: T)(implicit finder: TaggedFinder[T, X, Fields]): X with Tagging[T] =
      finder.find($fields)
  }

  def take[P, NewFields <: HList]
    (f: SelectContext[Fields, Sources] => P)
    (implicit
     generic: Generic.Aux[P, NewFields],
     taggedListOfFields: TagMap[Field[_], NewFields],
     queryBuilder: QueryBuilder[Select[NewFields, GroupBy, Sources, Where, Having, Sort], _, NewFields]):
    NonEmptySelect[NewFields, GroupBy, Sources, Where, Having, Sort] =
      new NonEmptySelect(generic.to(f(this)), $groupBy, $sources, $where, $having, $sortBy)

  def take1[F <: Field[_] with Tagging[_]]
    (f: SelectContext[Fields, Sources] => F)
    (implicit
     queryBuilder: QueryBuilder[Select[F :: HNil, GroupBy, Sources, Where, Having, Sort], _, F :: HNil]):
    NonEmptySelect[F :: HNil, GroupBy, Sources, Where, Having, Sort] =
      new NonEmptySelect(f(this) :: HNil, $groupBy, $sources, $where, $having, $sortBy)

  def groupBy[P, NewGroup <: HList]
    (f: SelectContext[Fields, Sources] => P)
    (implicit
     generic: Generic.Aux[P, NewGroup],
     taggedListOfFields: TagMap[Field[_], NewGroup],
     queryBuilder: QueryBuilder[Select[Fields, NewGroup, Sources, Where, Having, Sort], _, Fields]):
    NonEmptySelect[Fields, NewGroup, Sources, Where, Having, Sort] =
      new NonEmptySelect($fields, generic.to(f(this)), $sources, $where, $having, $sortBy)

  def groupBy1[F <: Field[_] with Tagging[_]]
    (f: SelectContext[Fields, Sources] => F)
    (implicit
     queryBuilder: QueryBuilder[Select[Fields, F :: HNil, Sources, Where, Having, Sort], _, Fields]):
    NonEmptySelect[Fields, F :: HNil, Sources, Where, Having, Sort] =
      new NonEmptySelect($fields, f(this) :: HNil, $sources, $where, $having, $sortBy)

  def innerJoin[RightSource <: Selectable[_] with Tagging[_], RightFields <: HList, RightAlias <: String with Singleton]
    (that: RightSource)
    (implicit
     selectableFields: SelectableFields[RightSource, RightFields],
     tagged: Tagged[RightSource, RightAlias]): JoinBuilder[Fields, GroupBy, Sources, Where, Having, Sort, RightSource, RightFields, RightAlias] =
    new JoinBuilder(this, that, selectableFields.fields(that), InnerJoin)

//  def leftJoin[NEW_Source <: Selectable[_] with Tagging[_], JOIN_Fields <: HList](that: NEW_Source)(implicit fields: NullableFields[]

  def where[NewWhere <: BinaryExpr]
    (f: SelectContext[Fields, Sources] => NewWhere)
    (implicit queryBuilder: QueryBuilder[Select[Fields, GroupBy, Sources, NewWhere, Having, Sort], _, Fields]): NonEmptySelect[Fields, GroupBy, Sources, NewWhere, Having, Sort] =
    new NonEmptySelect($fields, $groupBy, $sources, f(this), $having, $sortBy)

  def having[NewWhere <: BinaryExpr]
    (f: SelectContext[Fields, Sources] => NewWhere)
    (implicit queryBuilder: QueryBuilder[Select[Fields, GroupBy, Sources, Where, NewWhere, Sort], _, Fields]): NonEmptySelect[Fields, GroupBy, Sources, Where, NewWhere, Sort] =
    new NonEmptySelect($fields, $groupBy, $sources, $where, f(this), $sortBy)

  def sortBy[P, NewSort <: HList]
    (f: FindContext[Fields] => P)
    (implicit
     generic: Generic.Aux[P, NewSort],
     taggedListOfSortByClauses: Bounded[SortBy[_], NewSort],
     queryBuilder: QueryBuilder[Select[Fields, GroupBy, Sources, Where, Having, NewSort], _, Fields]): NonEmptySelect[Fields, GroupBy, Sources, Where, Having, NewSort] =
    new NonEmptySelect($fields, $groupBy, $sources, $where, $having, generic.to(f(findContext)))
}

object Select extends EmptySelect

