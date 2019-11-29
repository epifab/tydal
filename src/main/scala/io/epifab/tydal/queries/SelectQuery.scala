package io.epifab.tydal.queries

import io.epifab.tydal.schema._
import io.epifab.tydal.runtime.{ReadStatementStep1, StatementBuilder}
import io.epifab.tydal.utils.{Bounded, TaggedFinder}
import io.epifab.tydal.{As, TagMap, Tagged, Tagging}
import shapeless.ops.hlist.Tupler
import shapeless.{::, Generic, HList, HNil}

trait SelectContext[Fields <: HList, Sources <: HList] extends FindContext[(Fields, Sources)] {
  protected[tydal] def fields: Fields
  protected[tydal] def sources: Sources

  override def apply[T <: String with Singleton, X](tag: T)(implicit finder: TaggedFinder[T, X, (Fields, Sources)]): X with Tagging[T] =
    finder.find((fields, sources))

  def apply[T1 <: String with Singleton, T2 <: String with Singleton, X2, HAYSTACK2]
      (tag1: T1, tag2: T2)
      (implicit
       finder1: TaggedFinder[T1, FindContext[HAYSTACK2], Sources],
       finder2: TaggedFinder[T2, X2, HAYSTACK2]): X2 As T2 =
    finder1.find(sources).apply[T2, X2](tag2)
}

sealed class SelectQuery[Fields <: HList, GroupBy <: HList, Sources <: HList, Where <: BinaryExpr, Having <: BinaryExpr, Sort <: HList]
    (protected[tydal] val fields: Fields,
     protected[tydal] val groupBy: GroupBy,
     protected[tydal] val sources: Sources,
     protected[tydal] val where: Where,
     protected[tydal] val having: Having,
     protected[tydal] val sortBy: Sort)
    (implicit queryBuilder: QueryBuilder[SelectQuery[Fields, GroupBy, Sources, Where, Having, Sort], _, Fields])
    extends SelectContext[Fields, Sources] {

  private val findContext: FindContext[Fields] = new FindContext[Fields] {
    override def apply[T <: String with Singleton, X](tag: T)(implicit finder: TaggedFinder[T, X, Fields]): X with Tagging[T] =
      finder.find(fields)
  }

  def as[T <: String with Singleton with Singleton, SubQueryFieldsRepr <: HList](tag: T)(implicit subQueryFields: SubQueryFields[Fields, SubQueryFieldsRepr]): SelectSubQuery[SubQueryFieldsRepr, SelectQuery[Fields, GroupBy, Sources, Where, Having, Sort]] with Tagging[T] =
    new SelectSubQuery(this, subQueryFields.build(tag, fields)) with Tagging[T] {
      override def tagValue: String = tag
    }

  def compile[Placeholders <: HList, InputRepr <: HList, Input]
    (implicit
     queryBuilder: QueryBuilder[this.type, Placeholders, Fields],
     statementBuilder: StatementBuilder[Placeholders, InputRepr, Input, Fields],
     tupler: Tupler.Aux[InputRepr, Input]
    ): ReadStatementStep1[Input, Fields] =
    statementBuilder.build(queryBuilder.build(this)).select

  def from[S <: Selectable[_] with Tagging[_]]
    (source: S)
    (implicit
     ev: Sources =:= HNil.type,
     queryBuilder: QueryBuilder[SelectQuery[Fields, GroupBy, S :: HNil, Where, Having, Sort], _, Fields]
    ): SelectQuery[Fields, GroupBy, S :: HNil, Where, Having, Sort] =
    new SelectQuery(fields, groupBy, source :: HNil, where, having, sortBy)

  def take[P, NewFields <: HList]
    (f: SelectContext[Fields, Sources] => P)
    (implicit
     generic: Generic.Aux[P, NewFields],
     taggedListOfFields: TagMap[Field[_], NewFields],
     queryBuilder: QueryBuilder[SelectQuery[NewFields, GroupBy, Sources, Where, Having, Sort], _, NewFields]
    ): SelectQuery[NewFields, GroupBy, Sources, Where, Having, Sort] =
    new SelectQuery(generic.to(f(this)), groupBy, sources, where, having, sortBy)

  def take1[F <: Field[_] with Tagging[_]]
    (f: SelectContext[Fields, Sources] => F)
    (implicit
     queryBuilder: QueryBuilder[SelectQuery[F :: HNil, GroupBy, Sources, Where, Having, Sort], _, F :: HNil]
    ): SelectQuery[F :: HNil, GroupBy, Sources, Where, Having, Sort] =
    new SelectQuery(f(this) :: HNil, groupBy, sources, where, having, sortBy)

  def groupBy[P, NewGroup <: HList]
    (f: SelectContext[Fields, Sources] => P)
    (implicit
     generic: Generic.Aux[P, NewGroup],
     taggedListOfFields: TagMap[Field[_], NewGroup],
     queryBuilder: QueryBuilder[SelectQuery[Fields, NewGroup, Sources, Where, Having, Sort], _, Fields]
    ): SelectQuery[Fields, NewGroup, Sources, Where, Having, Sort] =
    new SelectQuery(fields, generic.to(f(this)), sources, where, having, sortBy)

  def groupBy1[F <: Field[_] with Tagging[_]]
    (f: SelectContext[Fields, Sources] => F)
    (implicit
     queryBuilder: QueryBuilder[SelectQuery[Fields, F :: HNil, Sources, Where, Having, Sort], _, Fields]
    ): SelectQuery[Fields, F :: HNil, Sources, Where, Having, Sort] =
    new SelectQuery(fields, f(this) :: HNil, sources, where, having, sortBy)

  def innerJoin[H, T <: HList, RightSource <: Selectable[_] with Tagging[_], RightFields <: HList, RightAlias <: String with Singleton]
    (that: RightSource)
    (implicit
     ev: Sources =:= (H :: T),
     fieldsOf: FieldsOf[RightSource, RightFields],
     tagged: Tagged[RightSource, RightAlias]
    ): JoinBuilder[Fields, GroupBy, Sources, Where, Having, Sort, RightSource, RightFields, RightAlias] =
    new JoinBuilder(this, that, fieldsOf(that), InnerJoin)

  def leftJoin[RightSource <: Selectable[_] with Tagging[_], RightFields <: HList, RightAlias <: String with Singleton, TargetFields <: HList]
    (that: RightSource)
    (implicit
     fieldsOf: FieldsOf[RightSource, RightFields],
     nullableFields: NullableFields[RightFields, TargetFields],
     tagged: Tagged[RightSource, RightAlias]
    ): JoinBuilder[Fields, GroupBy, Sources, Where, Having, Sort, RightSource, TargetFields, RightAlias] =
    new JoinBuilder(this, that, nullableFields.build(fieldsOf(that)), LeftJoin)

  def where[NewWhere <: BinaryExpr]
    (f: SelectContext[Fields, Sources] => NewWhere)
    (implicit
     queryBuilder: QueryBuilder[SelectQuery[Fields, GroupBy, Sources, NewWhere, Having, Sort], _, Fields]
    ): SelectQuery[Fields, GroupBy, Sources, NewWhere, Having, Sort] =
    new SelectQuery(fields, groupBy, sources, f(this), having, sortBy)

  def having[NewWhere <: BinaryExpr]
    (f: SelectContext[Fields, Sources] => NewWhere)
    (implicit
     queryBuilder: QueryBuilder[SelectQuery[Fields, GroupBy, Sources, Where, NewWhere, Sort], _, Fields]
    ): SelectQuery[Fields, GroupBy, Sources, Where, NewWhere, Sort] =
    new SelectQuery(fields, groupBy, sources, where, f(this), sortBy)

  def sortBy[P, NewSort <: HList]
    (f: FindContext[Fields] => P)
    (implicit
     generic: Generic.Aux[P, NewSort],
     taggedListOfSortByClauses: Bounded[SortBy[_], NewSort],
     queryBuilder: QueryBuilder[SelectQuery[Fields, GroupBy, Sources, Where, Having, NewSort], _, Fields]
    ): SelectQuery[Fields, GroupBy, Sources, Where, Having, NewSort] =
    new SelectQuery(fields, groupBy, sources, where, having, generic.to(f(findContext)))
}

object Select extends SelectQuery(HNil, HNil, HNil, AlwaysTrue, AlwaysTrue, HNil)
