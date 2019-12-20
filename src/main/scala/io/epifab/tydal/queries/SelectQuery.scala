package io.epifab.tydal.queries

import java.sql.Connection

import io.epifab.tydal.runtime.{ReadStatementExecutor, ReadStatementStep0, StatementBuilder, TagOutput}
import io.epifab.tydal.schema._
import io.epifab.tydal.utils.{Bounded, Concat, HSet, TaggedFind}
import io.epifab.tydal.{As, TagMap, Tagged, Tagging}
import shapeless.ops.hlist.Tupler
import shapeless.{::, Generic, HList, HNil}

trait SelectContext[Fields <: HList, Sources <: HList] extends FindContext[(Fields, Sources)] {
  private[tydal] def fields: Fields
  private[tydal] def sources: Sources

  override def apply[T <: String with Singleton, X](tag: T)(implicit find: TaggedFind[T, X, (Fields, Sources)]): X with Tagging[T] =
    find((fields, sources))

  def apply[T1 <: String with Singleton, T2 <: String with Singleton, X2, NestedContext](tag1: T1, tag2: T2)(
    implicit
    find1: TaggedFind[T1, FindContext[NestedContext], Sources],
    find2: TaggedFind[T2, X2, NestedContext]
  ): X2 As T2 =
    find1(sources).apply[T2, X2](tag2)
}

trait SelectOps[Fields <: HList, GroupBy <: HList, Sources <: HList, Where <: Filter, Having <: Filter, Sort <: HList, Context] {
  def select: SelectQuery[Fields, GroupBy, Sources, Where, Having, Sort]
  def context: Context

  def take[P <: Product, NewFields <: HList](f: Context => P)(
    implicit
    generic: Generic.Aux[P, NewFields],
    taggedListOfFields: TagMap[Field[_], NewFields],
    queryBuilder: QueryBuilder[SelectQuery[NewFields, GroupBy, Sources, Where, Having, Sort], _, NewFields]
  ): SelectQuery[NewFields, GroupBy, Sources, Where, Having, Sort] =
    new SelectQuery(generic.to(f(context)), select.groupBy, select.sources, select.where, select.having, select.sortBy, select.offset, select.limit)

  def take1[F <: Field[_] with Tagging[_]](f: Context => F)(
    implicit
    queryBuilder: QueryBuilder[SelectQuery[F :: HNil, GroupBy, Sources, Where, Having, Sort], _, F :: HNil]
  ): SelectQuery[F :: HNil, GroupBy, Sources, Where, Having, Sort] =
    new SelectQuery(f(context) :: HNil, select.groupBy, select.sources, select.where, select.having, select.sortBy, select.offset, select.limit)

  def alsoTake[P <: Product, NewFields <: HList, FinalFields <: HList](f: Context => P)(
    implicit
    generic: Generic.Aux[P, NewFields],
    taggedListOfFields: TagMap[Field[_], NewFields],
    concat: Concat.Aux[Fields, NewFields, FinalFields],
    queryBuilder: QueryBuilder[SelectQuery[FinalFields, GroupBy, Sources, Where, Having, Sort], _, FinalFields]
  ): SelectQuery[FinalFields, GroupBy, Sources, Where, Having, Sort] =
    new SelectQuery(concat(select.fields, generic.to(f(context))), select.groupBy, select.sources, select.where, select.having, select.sortBy, select.offset, select.limit)

  def where[NewWhere <: Filter](f: Context => NewWhere)(
    implicit
    queryBuilder: QueryBuilder[SelectQuery[Fields, GroupBy, Sources, NewWhere, Having, Sort], _, Fields]
  ): SelectQuery[Fields, GroupBy, Sources, NewWhere, Having, Sort] =
    new SelectQuery(select.fields, select.groupBy, select.sources, f(context), select.having, select.sortBy, select.offset, select.limit)

  def andWhere[NewWhere <: Filter](f: Context => NewWhere)(
    implicit
    queryBuilder: QueryBuilder[SelectQuery[Fields, GroupBy, Sources, And[Where, NewWhere], Having, Sort], _, Fields]
  ): SelectQuery[Fields, GroupBy, Sources, And[Where, NewWhere], Having, Sort] =
    new SelectQuery(select.fields, select.groupBy, select.sources, select.where and f(context), select.having, select.sortBy, select.offset, select.limit)

  def orWhere[NewWhere <: Filter](f: Context => NewWhere)(
    implicit
    queryBuilder: QueryBuilder[SelectQuery[Fields, GroupBy, Sources, Or[Where, NewWhere], Having, Sort], _, Fields]
  ): SelectQuery[Fields, GroupBy, Sources, Or[Where, NewWhere], Having, Sort] =
    new SelectQuery(select.fields, select.groupBy, select.sources, select.where or f(context), select.having, select.sortBy, select.offset, select.limit)

  def groupBy[P, RawGroup <: HList, Columns <: HList, NewGroup <: HList](f: Context => P)(
    implicit
    generic: Generic.Aux[P, RawGroup],
    extractColumns: GroupByColumns[RawGroup, Columns],
    hSet: HSet[Columns, NewGroup],
    queryBuilder: QueryBuilder[SelectQuery[Fields, NewGroup, Sources, Where, Having, Sort], _, Fields]
  ): SelectQuery[Fields, NewGroup, Sources, Where, Having, Sort] =
    new SelectQuery(select.fields, hSet.toSet(extractColumns(generic.to(f(context)))), select.sources, select.where, select.having, select.sortBy, select.offset, select.limit)

  def groupBy1[F <: Field[_] with Tagging[_]](f: Context => F)(
    implicit
    queryBuilder: QueryBuilder[SelectQuery[Fields, F :: HNil, Sources, Where, Having, Sort], _, Fields]
  ): SelectQuery[Fields, F :: HNil, Sources, Where, Having, Sort] =
    new SelectQuery(select.fields, f(context) :: HNil, select.sources, select.where, select.having, select.sortBy, select.offset, select.limit)

  def alsoGroupBy[P, RawGroup <: HList, Columns <: HList, NewGroup <: HList, FinalGroup <: HList](f: Context => P)(
    implicit
    generic: Generic.Aux[P, RawGroup],
    extractColumns: GroupByColumns[RawGroup, Columns],
    hSet: HSet[Columns, NewGroup],
    concat: Concat.Aux[GroupBy, NewGroup, FinalGroup],
    queryBuilder: QueryBuilder[SelectQuery[Fields, FinalGroup, Sources, Where, Having, Sort], _, Fields]
  ): SelectQuery[Fields, FinalGroup, Sources, Where, Having, Sort] =
    new SelectQuery(select.fields, concat(select.groupBy, hSet.toSet(extractColumns(generic.to(f(context))))), select.sources, select.where, select.having, select.sortBy, select.offset, select.limit)

  def having[NewHaving <: Filter](f: Context => NewHaving)(
    implicit
    queryBuilder: QueryBuilder[SelectQuery[Fields, GroupBy, Sources, Where, NewHaving, Sort], _, Fields]
  ): SelectQuery[Fields, GroupBy, Sources, Where, NewHaving, Sort] =
    new SelectQuery(select.fields, select.groupBy, select.sources, select.where, f(context), select.sortBy, select.offset, select.limit)

  def andHaving[NewHaving <: Filter](f: Context => NewHaving)(
    implicit
    queryBuilder: QueryBuilder[SelectQuery[Fields, GroupBy, Sources, Where, And[Having, NewHaving], Sort], _, Fields]
  ): SelectQuery[Fields, GroupBy, Sources, Where, And[Having, NewHaving], Sort] =
    new SelectQuery(select.fields, select.groupBy, select.sources, select.where, select.having and f(context), select.sortBy, select.offset, select.limit)

  def orHaving[NewHaving <: Filter](f: Context => NewHaving)(
    implicit
    queryBuilder: QueryBuilder[SelectQuery[Fields, GroupBy, Sources, Where, Or[Having, NewHaving], Sort], _, Fields]
  ): SelectQuery[Fields, GroupBy, Sources, Where, Or[Having, NewHaving], Sort] =
    new SelectQuery(select.fields, select.groupBy, select.sources, select.where, select.having or f(context), select.sortBy, select.offset, select.limit)
}

sealed class SelectQuery[Fields <: HList, GroupBy <: HList, Sources <: HList, Where <: Filter, Having <: Filter, Sort <: HList](
  private[tydal] val fields: Fields,
  private[tydal] val groupBy: GroupBy,
  private[tydal] val sources: Sources,
  private[tydal] val where: Where,
  private[tydal] val having: Having,
  private[tydal] val sortBy: Sort,
  private[tydal] val offset: Option[Long],
  private[tydal] val limit: Option[Int]
)(
  implicit
  queryBuilder: QueryBuilder[SelectQuery[Fields, GroupBy, Sources, Where, Having, Sort], _, Fields]
) extends SelectContext[Fields, Sources] with SelectOps[Fields, GroupBy, Sources, Where, Having, Sort, SelectContext[Fields, Sources]] { Self =>

  override val select: SelectQuery[Fields, GroupBy, Sources, Where, Having, Sort] = this
  override val context: SelectContext[Fields, Sources] = this

  def `*`[T](implicit tupler: Tupler.Aux[Fields, T]): T = tupler(fields)

  def as[T <: String with Singleton with Singleton, SubQueryFieldsRepr <: HList](tag: T)(implicit subQueryFields: SubQueryFields[Fields, SubQueryFieldsRepr]): SelectSubQuery[SubQueryFieldsRepr, SelectQuery[Fields, GroupBy, Sources, Where, Having, Sort]] with Tagging[T] =
    new SelectSubQuery(this, subQueryFields.build(tag, fields)) with Tagging[T] {
      override def tagValue: String = tag
    }

  def compile[Placeholders <: HList, InputRepr <: HList, Input, OutputRepr <: HList, TaggedOutput <: HList](
    implicit
    queryBuilder: QueryBuilder[this.type, Placeholders, Fields],
    statementBuilder: StatementBuilder[Placeholders, InputRepr, Input, Fields],
    tupler: Tupler.Aux[InputRepr, Input],
    readStatement: ReadStatementExecutor[Connection, Fields, OutputRepr],
    taggedOutput: TagOutput[Fields, OutputRepr, TaggedOutput]
  ): ReadStatementStep0[Input, Fields, OutputRepr, TaggedOutput] =
    statementBuilder.build(queryBuilder.build(this)).select

  def from[S <: Selectable[_] with Tagging[_]](source: S)(
    implicit
    ev: Sources =:= HNil.type,
    queryBuilder: QueryBuilder[SelectQuery[Fields, GroupBy, S :: HNil, Where, Having, Sort], _, Fields]
  ): SelectQuery[Fields, GroupBy, S :: HNil, Where, Having, Sort] =
    new SelectQuery(fields, groupBy, source :: HNil, where, having, sortBy, offset, limit)

  def innerJoin[H, T <: HList, RightSource <: Selectable[_] with Tagging[_], RightFields <: HList, RightAlias <: String with Singleton](that: RightSource)(
    implicit
    ev: Sources =:= (H :: T),
    fieldsOf: FieldsOf[RightSource, RightFields],
    tagged: Tagged[RightSource, RightAlias]
  ): JoinBuilder[Fields, GroupBy, Sources, Where, Having, Sort, RightSource, RightFields, RightAlias] =
    new JoinBuilder(this, that, fieldsOf(that), InnerJoin)

  def leftJoin[RightSource <: Selectable[_] with Tagging[_], RightFields <: HList, RightAlias <: String with Singleton, TargetFields <: HList](that: RightSource)(
    implicit
    fieldsOf: FieldsOf[RightSource, RightFields],
    nullableFields: NullableFields[RightFields, TargetFields],
    tagged: Tagged[RightSource, RightAlias]
  ): JoinBuilder[Fields, GroupBy, Sources, Where, Having, Sort, RightSource, TargetFields, RightAlias] =
    new JoinBuilder(this, that, nullableFields.build(fieldsOf(that)), LeftJoin)

  def sortBy[P, NewSort <: HList](f: SelectContext[Fields, Sources] => P)(
    implicit
    generic: Generic.Aux[P, NewSort],
    taggedListOfSortByClauses: Bounded[SortBy[_], NewSort],
    queryBuilder: QueryBuilder[SelectQuery[Fields, GroupBy, Sources, Where, Having, NewSort], _, Fields]
  ): SelectQuery[Fields, GroupBy, Sources, Where, Having, NewSort] =
    new SelectQuery(fields, groupBy, sources, where, having, generic.to(f(this)), offset, limit)

  def inRange(newOffset: Long, newLimit: Int): SelectQuery[Fields, GroupBy, Sources, Where, Having, Sort] =
    new SelectQuery(fields, groupBy, sources, where, having, sortBy, Some(newOffset), Some(newLimit))

  private def withFocus[Source](focus: Source): SelectOps[Fields, GroupBy, Sources, Where, Having, Sort, Source] =
    new SelectOps[Fields, GroupBy, Sources, Where, Having, Sort, Source] {
      override def select: SelectQuery[Fields, GroupBy, Sources, Where, Having, Sort] = Self
      override def context: Source = focus
    }

  def focus[
    X <: String with Singleton,
    S <: FindContext[_]
  ](x: X)(
    implicit
    find: TaggedFind[X, S, Sources]
  ): SelectOps[Fields, GroupBy, Sources, Where, Having, Sort, As[S, X]] =
    withFocus(find(sources))

  def focus[
    X1 <: String with Singleton,
    X2 <: String with Singleton,
    S1 <: FindContext[_],
    S2 <: FindContext[_]
  ](x1: X1, x2: X2)(
    implicit
    find1: TaggedFind[X1, S1, Sources],
    find2: TaggedFind[X2, S2, Sources],
  ): SelectOps[Fields, GroupBy, Sources, Where, Having, Sort, (As[S1, X1], As[S2, X2])] =
    withFocus((find1(sources), find2(sources)))

  def focus[
    X1 <: String with Singleton,
    X2 <: String with Singleton,
    X3 <: String with Singleton,
    S1 <: FindContext[_],
    S2 <: FindContext[_],
    S3 <: FindContext[_]
  ](x1: X1, x2: X2, x3: X3)(
    implicit
    find1: TaggedFind[X1, S1, Sources],
    find2: TaggedFind[X2, S2, Sources],
    find3: TaggedFind[X3, S3, Sources],
  ): SelectOps[Fields, GroupBy, Sources, Where, Having, Sort, (As[S1, X1], As[S2, X2], As[S3, X3])] =
    withFocus((find1(sources), find2(sources), find3(sources)))
}

object Select extends SelectQuery(HNil, HNil, HNil, AlwaysTrue, AlwaysTrue, HNil, None, None)
