package io.epifab.tydal.queries

import io.epifab.tydal._
import io.epifab.tydal.queries.Ascending.AscendingOrder
import io.epifab.tydal.queries.Descending.DescendingOrder
import io.epifab.tydal.queries.FragmentType._
import io.epifab.tydal.schema._
import io.epifab.tydal.utils.Concat
import shapeless.{::, HList, HNil}

import scala.annotation.implicitNotFound

case class CompiledQuery[+Placeholders <: HList, +Fields <: HList](sql: String, placeholders: Placeholders, fields: Fields)

case class CompiledQueryFragment[+P <: HList](sql: Option[String], placeholders: P)

object CompiledQueryFragment {

  def empty: CompiledQueryFragment[HNil] = CompiledQueryFragment(None, HNil)

  def apply[P <: HList](query: CompiledQuery[P, _]): CompiledQueryFragment[P] =
    CompiledQueryFragment(Some(query.sql), query.placeholders)

  def apply(sql: String): CompiledQueryFragment[HNil] =
    CompiledQueryFragment(Some(sql), HNil)

  def apply[P <: Placeholder[_]](sql: String, placeholder: P): CompiledQueryFragment[P :: HNil] =
    CompiledQueryFragment(Some(sql), placeholder :: HNil)

  implicit class ExtendedQueryFragment[P <: HList](queryFragment: CompiledQueryFragment[P]) {
    def mapPlaceholders[Q <: HList](f: P => Q): CompiledQueryFragment[Q] =
      CompiledQueryFragment(queryFragment.sql, f(queryFragment.placeholders))

    def `++`(s: String): CompiledQueryFragment[P] =
      append(s)

    def `++`[Q <: HList, R <: HList](other: CompiledQueryFragment[Q])(implicit concat: Concat.Aux[P, Q, R]): CompiledQueryFragment[R] =
      concatenateOptional(other, "")

    def `+ +`[Q <: HList, R <: HList](other: CompiledQueryFragment[Q])(implicit concat: Concat.Aux[P, Q, R]): CompiledQueryFragment[R] =
      concatenateOptional(other, " ")

    def `+,+`[Q <: HList, R <: HList](other: CompiledQueryFragment[Q])(implicit concat: Concat.Aux[P, Q, R]): CompiledQueryFragment[R] =
      concatenateOptional(other, ", ")

    def wrap(before: String, after: String): CompiledQueryFragment[P] =
      CompiledQueryFragment(queryFragment.sql.map(before + _ + after), queryFragment.placeholders)

    def append(after: String): CompiledQueryFragment[P] =
      CompiledQueryFragment(queryFragment.sql.map(_ + after), queryFragment.placeholders)

    def prepend(before: String): CompiledQueryFragment[P] =
      CompiledQueryFragment(queryFragment.sql.map(before + _), queryFragment.placeholders)

    def map(f: String => String): CompiledQueryFragment[P] =
      CompiledQueryFragment(queryFragment.sql.map(f), queryFragment.placeholders)

    def concatenateOptional[Q <: HList, R <: HList](other: CompiledQueryFragment[Q], separator: String)(implicit concat: Concat.Aux[P, Q, R]): CompiledQueryFragment[R] =
      CompiledQueryFragment(
        Seq(queryFragment.sql, other.sql)
          .flatten
          .reduceOption(_ ++ separator ++ _),
        concat(queryFragment.placeholders, other.placeholders)
      )

    def concatenateRequired[Q <: HList, R <: HList](other: CompiledQueryFragment[Q], separator: String)(implicit concat: Concat.Aux[P, Q, R]): CompiledQueryFragment[R] =
      CompiledQueryFragment(
        for {
          s1 <- queryFragment.sql
          s2 <- other.sql
        } yield s1 + separator + s2,
        concat(queryFragment.placeholders, other.placeholders)
      )

    def getOrElse[Fields <: HList](default: String, fields: Fields): CompiledQuery[P, Fields] =
      CompiledQuery(queryFragment.sql.getOrElse(default), queryFragment.placeholders, fields)

    def orElse(s: Option[String]): CompiledQueryFragment[P] =
      new CompiledQueryFragment(queryFragment.sql.orElse(s), queryFragment.placeholders)

    def get[Fields <: HList](fields: Fields): CompiledQuery[P, Fields] =
      CompiledQuery(queryFragment.sql.get, queryFragment.placeholders, fields)
  }
}

@implicitNotFound("Could not find a query builder for ${X}")
sealed trait QueryBuilder[-X, P <: HList, F <: HList] {
  def build(x: X): CompiledQuery[P, F]
}

object QueryBuilder {
  def apply[X, P <: HList, F <: HList](x: X)(implicit queryBuilder: QueryBuilder[X, P, F]): CompiledQuery[P, F] =
    queryBuilder.build(x)

  def instance[T, P <: HList, F <: HList](f: T => CompiledQuery[P, F]): QueryBuilder[T, P, F] =
    new QueryBuilder[T, P, F] {
      override def build(x: T): CompiledQuery[P, F] = f(x)
    }

  implicit def selectQuery[
    Fields <: HList, GroupBy <: HList, Sources <: HList, Where <: Filter, Having <: Filter, Sort <: HList, Offset, Limit,
    P1 <: HList, P2 <: HList, P3 <: HList, P4 <: HList, P5 <: HList,
    P6 <: HList, P7 <: HList, P8 <: HList, P9 <: HList, P10 <: HList,
    P11 <: HList, P12 <: HList, P13 <: HList, P14 <: HList, P15 <: HList
  ]
    (implicit
     fields: QueryFragmentBuilder[FT_FieldExprAndAliasList, Fields, P1],
     from: QueryFragmentBuilder[FT_From, Sources, P2],
     concat1: Concat.Aux[P1, P2, P3],
     where: QueryFragmentBuilder[FT_Where, Where, P4],
     concat2: Concat.Aux[P3, P4, P5],
     groupBy: QueryFragmentBuilder[FT_FieldExprList, GroupBy, P6],
     concat3: Concat.Aux[P5, P6, P7],
     having: QueryFragmentBuilder[FT_Where, Having, P8],
     concat4: Concat.Aux[P7, P8, P9],
     sortBy: QueryFragmentBuilder[FT_SortBy, Sort, P10],
     concat5: Concat.Aux[P9, P10, P11],
     offset: QueryFragmentBuilder[FT_OffsetPlaceholder, Offset, P12],
     concat6: Concat.Aux[P11, P12, P13],
     limit: QueryFragmentBuilder[FT_LimitPlaceholder, Limit, P14],
     concat7: Concat.Aux[P13, P14, P15]
    ): QueryBuilder[SelectQuery[Fields, GroupBy, Sources, Where, Having, Sort, Offset, Limit], P15, Fields] =
    QueryBuilder.instance(select =>
      (fields.build(select.fields).orElse(Some("1")).prepend("SELECT ") `+ +`
        from.build(select.sources).prepend("FROM ") `+ +`
        where.build(select.where).prepend("WHERE ") `+ +`
        groupBy.build(select.groupBy).prepend("GROUP BY ") `+ +`
        having.build(select.having).prepend("HAVING ") `+ +`
        sortBy.build(select.sortBy).prepend("ORDER BY ") `+ +`
        offset.build(select.offset).prepend("OFFSET ") `+ +`
        limit.build(select.limit).prepend("LIMIT ")
      ).get(select.fields)
    )

  implicit def insertQuery[Fields <: HList, Values <: HList, ConflictPolicy <: OnConflict, P1 <: HList, P2 <: HList, P3 <: HList]
      (implicit
       names: QueryFragmentBuilder[FT_AliasList, Fields, HNil],
       values: QueryFragmentBuilder[FT_FieldExprList, Values, P1],
       onConflict: QueryFragmentBuilder[FT_ConflictPolicy, ConflictPolicy, P2],
       concat: Concat.Aux[P1, P2, P3]
      ): QueryBuilder[InsertQuery[Fields, Values, ConflictPolicy], P3, HNil] =
    QueryBuilder.instance(insert => {
      (names.build(insert.table.fields).wrap("(", ")") ++
        " VALUES " ++
        values.build(insert.values).wrap("(", ")") ++
        onConflict.build(insert.onConflict)
      ).prepend(s"INSERT INTO ${insert.table.tableName} ").get(HNil)
    })

  implicit def updateQuery[Fields <: HList, Values <: HList, P <: HList, Q <: HList, R <: HList, Where <: Filter]
      (implicit
       values: QueryFragmentBuilder[FT_FieldAliasEqualExprList, Values, P],
       where: QueryFragmentBuilder[FT_Where, Where, Q],
       concat: Concat.Aux[P, Q, R]
      ): QueryBuilder[UpdateQuery[Fields, Values, Where], R, HNil] =
    QueryBuilder.instance(update => {
      (values.build(update.values).prepend("UPDATE " + update.table.tableName + " SET ") ++
        where.build(update.where).prepend(" WHERE ")).get(HNil)
    })

  implicit def deleteQuery[TableName <: String with Singleton, Schema <: HList, P <: HList, Where <: Filter]
      (implicit
       where: QueryFragmentBuilder[FT_Where, Where, P]
      ): QueryBuilder[DeleteQuery[Schema, Where], P, HNil] =
    QueryBuilder.instance(delete =>
      (CompiledQueryFragment(s"DELETE FROM ${delete.table.tableName}") ++
      where.build(delete.filter).prepend(" WHERE "))
          .get(HNil)
    )
}

sealed trait QueryFragmentBuilder[Alias, -Target, Placeholders <: HList] {
  def build(x: Target): CompiledQueryFragment[Placeholders]
}

object FragmentType {
  type FT_FieldExprAndAliasList = "FieldExprAndAliasList"
  type FT_FieldExprList = "FieldExprList"
  type FT_FieldAliasEqualExprList = "FieldAliasEqualExprList"
  type FT_From = "From"
  type FT_Where = "FT_Where"
  type FT_AliasList = "AliasList"
  type FT_SortBy = "SortByList"
  type FT_LimitPlaceholder = "FT_LimitPlaceholder"
  type FT_OffsetPlaceholder = "FT_OffsetPlaceholder"
  type FT_ConflictPolicy = "FT_ConflictPolicy"
}

object QueryFragmentBuilder {
  class PartialQFB[FT] {
    def apply[T, P <: HList](t: T)(implicit queryFragmentBuilder: QueryFragmentBuilder[FT, T, P]): QueryFragmentBuilder[FT, T, P] =
      queryFragmentBuilder
  }

  def apply[FT] = new PartialQFB[FT]

  def instance[A, T, P <: HList](f: T => CompiledQueryFragment[P]): QueryFragmentBuilder[A, T, P] =
    new QueryFragmentBuilder[A, T, P] {
      override def build(x: T): CompiledQueryFragment[P] = f(x)
    }

  implicit def fromEmptySources: QueryFragmentBuilder[FT_From, HNil, HNil] =
    QueryFragmentBuilder.instance(_ => CompiledQueryFragment.empty)

  implicit def fromNonEmptySources[H, T <: HList, P <: HList, Q <: HList, R <: HList]
      (implicit
       head: QueryFragmentBuilder[FT_From, H, P],
       tail: QueryFragmentBuilder[FT_From, T, Q],
       concat: Concat.Aux[P, Q, R]): QueryFragmentBuilder[FT_From, H :: T, R] =
    instance { sources =>
      head.build(sources.head) `+ +` tail.build(sources.tail)
    }

  implicit def fromJoin[S <: Selectable[_] with Tagging[_], JoinClause <: Filter, Fields <: HList, A <: String with Singleton, P <: HList, Q <: HList, R <: HList]
      (implicit
       src: QueryFragmentBuilder[FT_From, S, P],
       where: QueryFragmentBuilder[FT_Where, JoinClause, Q],
       concat: Concat.Aux[P, Q, R]): QueryFragmentBuilder[FT_From, Join[S, Fields, JoinClause], R] =
    instance(join =>
      src.build(join.right).prepend(join.joinType match {
        case InnerJoin => "INNER JOIN "
        case LeftJoin => "LEFT JOIN "
      }) ++
        where.build(join.joinClause).prepend(" ON ")
    )

  implicit val fromTable: QueryFragmentBuilder[FT_From, Table[_] with Tagging[_], HNil] =
    QueryFragmentBuilder.instance(table => CompiledQueryFragment(table.tableName + " AS " + table.tagValue))

  implicit def fromSubQuery[SubQueryFields <: HList, S <: SelectQuery[_, _, _, _, _, _, _, _], P <: HList]
      (implicit query: QueryBuilder[S, P, _]): QueryFragmentBuilder[FT_From, SelectSubQuery[SubQueryFields, S] with Tagging[_], P] =
    instance(subQuery =>
      CompiledQueryFragment(query.build(subQuery.select))
        .wrap("(", ") AS " + subQuery.tagValue)
    )

  // ------------------------------
  // expr as alias, ...
  // ------------------------------

  implicit def fieldExprAndAliasField[P <: HList, F <: Tagging[_]](implicit src: QueryFragmentBuilder[FT_FieldExprList, F, P]): QueryFragmentBuilder[FT_FieldExprAndAliasList, F, P] =
    instance((f: F) => src.build(f).append(" AS " + f.tagValue))

  implicit def fieldExprAndAliasEmptyList: QueryFragmentBuilder[FT_FieldExprAndAliasList, HNil, HNil] =
    QueryFragmentBuilder.instance(_ => CompiledQueryFragment.empty)

  implicit def fieldExprAndAliasNonEmptyList[H, T <: HList, P <: HList, Q <: HList, R <: HList]
      (implicit
       head: QueryFragmentBuilder[FT_FieldExprAndAliasList, H, P],
       tail: QueryFragmentBuilder[FT_FieldExprAndAliasList, T, Q],
       concat: Concat.Aux[P, Q, R]): QueryFragmentBuilder[FT_FieldExprAndAliasList, H :: T, R] =
    QueryFragmentBuilder.instance(fields =>
      head.build(fields.head) `+,+` tail.build(fields.tail)
    )

  // ------------------------------
  // alias = expr, ...
  // ------------------------------

  implicit def fieldAliasEqualExprField[P <: HList, F <: Tagging[_]](implicit src: QueryFragmentBuilder[FT_FieldExprList, F, P]): QueryFragmentBuilder[FT_FieldAliasEqualExprList, F, P] =
    instance((f: F) => src.build(f).prepend(f.tagValue + " = "))

  implicit def fieldAliasEqualExprEmptyList: QueryFragmentBuilder[FT_FieldAliasEqualExprList, HNil, HNil] =
    QueryFragmentBuilder.instance(_ => CompiledQueryFragment.empty)

  implicit def fieldAliasEqualExprNonEmptyList[H, T <: HList, P <: HList, Q <: HList, R <: HList]
      (implicit
       head: QueryFragmentBuilder[FT_FieldAliasEqualExprList, H, P],
       tail: QueryFragmentBuilder[FT_FieldAliasEqualExprList, T, Q],
       concat: Concat.Aux[P, Q, R]): QueryFragmentBuilder[FT_FieldAliasEqualExprList, H :: T, R] =
    QueryFragmentBuilder.instance(fields =>
      head.build(fields.head) `+,+` tail.build(fields.tail)
    )

  // ------------------------------
  // epxr1, epxr2, ...
  // ------------------------------

  implicit def fieldExprEmptyList: QueryFragmentBuilder[FT_FieldExprList, HNil, HNil] =
    QueryFragmentBuilder.instance(_ => CompiledQueryFragment.empty)

  implicit def fieldExprNonEmptyList[H, T <: HList, P <: HList, Q <: HList, R <: HList]
      (implicit
       head: QueryFragmentBuilder[FT_FieldExprList, H, P],
       tail: QueryFragmentBuilder[FT_FieldExprList, T, Q],
       concat: Concat.Aux[P, Q, R]): QueryFragmentBuilder[FT_FieldExprList, H :: T, R] =
    QueryFragmentBuilder.instance(fields =>
      head.build(fields.head) `+,+` tail.build(fields.tail)
    )

  // ------------------------------
  // Sort by
  // ------------------------------

//  implicit def sortByFieldWithAlias[F <: Field[_] with Tagging[_], P <: HList](
//    implicit
//    field: QueryFragmentBuilder[FT_AliasList, F, P]
//  ): QueryFragmentBuilder[FT_SortBy, SortBy[F], P] =
//    QueryFragmentBuilder.instance(sortBy =>
//      field.build(sortBy.field).append(" " + (sortBy.sortOrder match {
//        case AscendingOrder => "ASC"
//        case DescendingOrder => "DESC"
//      })))

  implicit def sortByFieldWithoutAlias[F <: Field[_], P <: HList](
    implicit
    field: QueryFragmentBuilder[FT_FieldExprList, F, P],
    // todo: it's neater to use field aliases whenever possible here, but they are don't necessarily exists in the
    //  actual query, e.g. NamedPlaceholder[T, "a"], they won't exists unless the placeholder is selected
    // untagged: Untagged[F]
  ): QueryFragmentBuilder[FT_SortBy, SortBy[F], P] =
    QueryFragmentBuilder.instance(sortBy =>
      field.build(sortBy.field).append(" " + (sortBy.sortOrder match {
        case AscendingOrder => "ASC"
        case DescendingOrder => "DESC"
      })))


  implicit def sortByEmptyList: QueryFragmentBuilder[FT_SortBy, HNil, HNil] =
    QueryFragmentBuilder.instance(_ => CompiledQueryFragment.empty)

  implicit def sortByNonEmptyList[H, T <: HList, P <: HList, Q <: HList, R <: HList](
    implicit
    head: QueryFragmentBuilder[FT_SortBy, H, P],
    tail: QueryFragmentBuilder[FT_SortBy, T, Q],
    concat: Concat.Aux[P, Q, R]
  ): QueryFragmentBuilder[FT_SortBy, H :: T, R] =
    instance(clauses => head.build(clauses.head) `+,+` tail.build(clauses.tail))

  // ------------------------------
  // expr
  // ------------------------------

  implicit val fieldExprColumn: QueryFragmentBuilder[FT_FieldExprList, Column[_], HNil] =
    instance((f: Column[_]) => CompiledQueryFragment(s"${f.relationAlias}.${f.name}"))

  implicit def fieldExprCast[F <: Field[_], P <: HList](implicit builder: QueryFragmentBuilder[FT_FieldExprList, F, P]): QueryFragmentBuilder[FT_FieldExprList, Cast[F, _], P] =
    instance((f: Cast[F, _]) => builder.build(f.field).append("::" + f.decoder.dbType.sqlName))

  implicit def fieldExprSoftCast[F <: Field[_], P <: HList](implicit builder: QueryFragmentBuilder[FT_FieldExprList, F, P]): QueryFragmentBuilder[FT_FieldExprList, SoftCast[F, _], P] =
    instance((f: SoftCast[F, _]) => builder.build(f.field))

  implicit def fieldExprAggregation[F <: Field[_], P <: HList](implicit builder: QueryFragmentBuilder[FT_FieldExprList, F, P]): QueryFragmentBuilder[FT_FieldExprList, Aggregation[F, _], P] =
    instance((f: Aggregation[F, _]) => builder.build(f.field).wrap(f.dbFunction.name + "(", ")"))

  implicit def fieldExpr1[F <: Field[_], P <: HList](implicit builder: QueryFragmentBuilder[FT_FieldExprList, F, P]): QueryFragmentBuilder[FT_FieldExprList, FieldExpr1[F, _], P] =
    instance((f: FieldExpr1[F, _]) => builder.build(f.field).wrap(f.dbFunction.name + "(", ")"))

  implicit def fieldExpr2[F1 <: Field[_], F2 <: Field[_], P <: HList, Q <: HList, R <: HList]
      (implicit
       builder1: QueryFragmentBuilder[FT_FieldExprList, F1, P],
       builder2: QueryFragmentBuilder[FT_FieldExprList, F2, Q],
       concat: Concat.Aux[P, Q, R]): QueryFragmentBuilder[FT_FieldExprList, FieldExpr2[F1, F2, _], R] =
    instance((f: FieldExpr2[F1, F2, _]) => (builder1.build(f.field1) `+,+` builder2.build(f.field2)).wrap(f.dbFunction.name + "(", ")"))

  implicit def placeholder[P <: NamedPlaceholder[_]]: QueryFragmentBuilder[FT_FieldExprList, P, P :: HNil] =
    instance((f: P) => CompiledQueryFragment(s"?::${f.encoder.dbType.sqlName}", f))

  implicit def value[V <: Literal[_]]: QueryFragmentBuilder[FT_FieldExprList, V, V :: HNil] =
    instance((f: V) => CompiledQueryFragment(s"?::${f.encoder.dbType.sqlName}", f))

  implicit def optionalValue[V <: LiteralOption[_]]: QueryFragmentBuilder[FT_FieldExprList, V, V :: HNil] =
    instance((f: V) => CompiledQueryFragment(f.value.map(_ => s"?::${f.encoder.dbType.sqlName}"), f :: HNil))

  // ------------------------------
  // alias1, alias2, ...
  // ------------------------------

  implicit val aliasSingleElement: QueryFragmentBuilder[FT_AliasList, Tagging[_], HNil] =
    instance((f: Tagging[_]) => CompiledQueryFragment(f.tagValue))

  implicit def aliasNoElements: QueryFragmentBuilder[FT_AliasList, HNil, HNil] =
    QueryFragmentBuilder.instance(_ => CompiledQueryFragment.empty)

  implicit def aliasMultipleElements[H, T <: HList, P <: HList, Q <: HList, R <: HList]
      (implicit
       head: QueryFragmentBuilder[FT_AliasList, H, P],
       tail: QueryFragmentBuilder[FT_AliasList, T, Q],
       concat: Concat.Aux[P, Q, R]): QueryFragmentBuilder[FT_AliasList, H :: T, R] =
    instance { sources =>
      head.build(sources.head) `+,+` tail.build(sources.tail)
    }

  // ------------------------------
  // Filters
  // ------------------------------

  implicit def whereAlwaysTrue: QueryFragmentBuilder[FT_Where, AlwaysTrue, HNil] =
    instance((_: AlwaysTrue) => CompiledQueryFragment.empty)

  implicit def whereFilterOption[F <: Filter, P <: HList, Q <: HList](
    implicit
    where: QueryFragmentBuilder[FT_Where, F, P],
    literalOptions: LiteralOptions[P, Q]
  ): QueryFragmentBuilder[FT_Where, FilterOption[F], Q] =
    instance((filter: FilterOption[F]) => filter.filter match {
      case Some(e) => where.build(e).mapPlaceholders(literalOptions.build)
      case None => CompiledQueryFragment(None, literalOptions.empty)
    })

  implicit def whereFilter1[F, P <: HList]
      (implicit left: QueryFragmentBuilder[FT_Where, F, P]): QueryFragmentBuilder[FT_Where, Filter1[F], P] =
    instance { filter: Filter1[F] =>
      val e1 = left.build(filter.field)
      filter match {
        case _: IsDefined[_] => e1 ++ " IS NOT NULL"
        case _: IsNotDefined[_] => e1 ++ " IS NULL"
      }
    }

  implicit def whereFilter2[F1, F2, P <: HList, Q <: HList, R <: HList]
      (implicit
       left: QueryFragmentBuilder[FT_Where, F1, P],
       right: QueryFragmentBuilder[FT_Where, F2, Q],
       concat: Concat.Aux[P, Q, R]): QueryFragmentBuilder[FT_Where, Filter2[F1, F2], R] =
    instance { filter: Filter2[F1, F2] =>
      val e1 = left.build(filter.left)
      val e2 = right.build(filter.right)
      filter match {
        case _: And[_, _] => e1.concatenateOptional(e2, " AND ")
        case _: Or[_, _] => e1.concatenateOptional(e2, " OR ").wrap("(", ")")
        case _: Equals[_, _] => e1.concatenateRequired(e2, " = ")
        case _: NotEquals[_, _] => e1.concatenateRequired(e2, " != ")
        case _: GreaterThan[_, _] => e1.concatenateRequired(e2, " > ")
        case _: LessThan[_, _] => e1.concatenateRequired(e2, " < ")
        case _: GreaterThanOrEqual[_, _] => e1.concatenateRequired(e2, " >= ")
        case _: LessThanOrEqual[_, _] => e1.concatenateRequired(e2, " <= ")
        case _: Like[_, _] => e1.concatenateRequired(e2, " LIKE ")
        case _: ILike[_, _] => e1.concatenateRequired(e2, " ILIKE ")
        case _: IsSubset[_, _] => e1.concatenateRequired(e2, " <@ ")
        case _: IsSuperset[_, _] => e1.concatenateRequired(e2, " @> ")
        case _: Overlaps[_, _] => e1.concatenateRequired(e2, " && ")
        case _: IsIncluded[_, _] => e1.concatenateRequired(e2.wrap("(", ")"), " = ANY")
        case _: InSubquery[_, _, _, _, _, _, _, _, _] => e1.concatenateRequired(e2.wrap("(", ")"), " IN ")
      }
    }

  implicit def whereField[P <: HList, F <: Field[_]](implicit field: QueryFragmentBuilder[FT_FieldExprList, F, P]): QueryFragmentBuilder[FT_Where, F, P] =
    instance(field.build)

  implicit def whereSubQuery[P <: HList, S <: SelectQuery[_, _, _, _, _, _, _, _]](implicit subQuery: QueryBuilder[S, P, _]): QueryFragmentBuilder[FT_Where, S, P] =
    instance(s => CompiledQueryFragment(subQuery.build(s)))

  // ------------------------------
  // Offset and limit
  // ------------------------------

  implicit def offset[P <: NamedPlaceholder[_]]: QueryFragmentBuilder[FT_OffsetPlaceholder, P, P :: HNil] =
    instance((f: P) => CompiledQueryFragment(s"?::${f.encoder.dbType.sqlName}", f))

  implicit val noOffset: QueryFragmentBuilder[FT_OffsetPlaceholder, HNil, HNil] =
    instance(_ => CompiledQueryFragment.empty)

  implicit def limit[P <: NamedPlaceholder[_]]: QueryFragmentBuilder[FT_LimitPlaceholder, P, P :: HNil] =
    instance((f: P) => CompiledQueryFragment(s"?::${f.encoder.dbType.sqlName}", f))

  implicit val noLimit: QueryFragmentBuilder[FT_LimitPlaceholder, HNil, HNil] =
    instance(_ => CompiledQueryFragment.empty)

  // ------------------------------
  // Conflict policy
  // ------------------------------

  implicit def onConflictThrowException: QueryFragmentBuilder[FT_ConflictPolicy, OnConflict.ThrowException, HNil] =
    instance(_ => CompiledQueryFragment.empty)

  implicit def onConflictDoNothing[ConflictFields <: HList, P <: HList](implicit conflictFields: QueryFragmentBuilder[FT_AliasList, ConflictFields, P]): QueryFragmentBuilder[FT_ConflictPolicy, OnConflict.DoNothing[ConflictFields], P] =
    instance(x => CompiledQueryFragment(s" ON CONFLICT ") ++ conflictFields.build(x.fields).wrap("(", ")") ++ " DO NOTHING")

  implicit def onConflictDoUpdate[ConflictFields <: HList, UpdatePlaceholders <: HList, P1 <: HList, P2 <: HList, P3 <: HList](
      implicit
      conflictFields: QueryFragmentBuilder[FT_AliasList, ConflictFields, P1],
      updatePlaceholders: QueryFragmentBuilder[FT_FieldAliasEqualExprList, UpdatePlaceholders, P2],
      concat: Concat.Aux[P1, P2, P3]
    ): QueryFragmentBuilder[FT_ConflictPolicy, OnConflict.DoUpdate[ConflictFields, UpdatePlaceholders], P3] =
    instance(onConflict => CompiledQueryFragment(s" ON CONFLICT ") ++ conflictFields.build(onConflict.fields).wrap("(", ")") ++ " DO UPDATE SET " ++ updatePlaceholders.build(onConflict.updatePlaceholders))

}
