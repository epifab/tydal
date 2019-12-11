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
      CompiledQueryFragment.apply(
        Seq(queryFragment.sql, other.sql)
          .flatten
          .reduceOption(_ ++ separator ++ _),
        concat(queryFragment.placeholders, other.placeholders)
      )

    def concatenateRequired[Q <: HList, R <: HList](other: CompiledQueryFragment[Q], separator: String)(implicit concat: Concat.Aux[P, Q, R]): CompiledQueryFragment[R] =
      CompiledQueryFragment.apply(
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
    Fields <: HList, GroupBy <: HList, Sources <: HList, Where <: Filter, Having <: Filter, Sort <: HList,
    P1 <: HList, P2 <: HList, P3 <: HList, P4 <: HList, P5 <: HList,
    P6 <: HList, P7 <: HList, P8 <: HList, P9 <: HList, P10 <: HList, P11 <: HList
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
     trivialConcat: Concat.Aux[P11, HNil, P11]
    ): QueryBuilder[SelectQuery[Fields, GroupBy, Sources, Where, Having, Sort], P11, Fields] =
    QueryBuilder.instance(select =>
      (fields.build(select.fields).orElse(Some("1")).prepend("SELECT ") `+ +`
        from.build(select.sources).prepend("FROM ") `+ +`
        where.build(select.where).prepend("WHERE ") `+ +`
        groupBy.build(select.groupBy).prepend("GROUP BY ") `+ +`
        having.build(select.having).prepend("Having ") `+ +`
        sortBy.build(select.sortBy).prepend("ORDER BY ") `+ +`
        CompiledQueryFragment[HNil](select.offset.map(offset => s"OFFSET $offset"), HNil) `+ +`
        CompiledQueryFragment[HNil](select.limit.map(limit => s"LIMIT $limit"), HNil)
      ).get(select.fields)
    )

  implicit def insertQuery[Columns <: HList, Values <: HList, Placeholders <: HList]
      (implicit
       names: QueryFragmentBuilder[FT_ColumnNameList, Columns, HNil],
       values: QueryFragmentBuilder[FT_FieldExprList, Values, Placeholders]): QueryBuilder[InsertQuery[Columns, Values], Placeholders, HNil] =
    QueryBuilder.instance(insert => {
      (names.build(insert.table.fields).wrap("(", ")") ++
        " VALUES " ++
        values.build(insert.values).wrap("(", ")")
      ).prepend(s"INSERT INTO ${insert.table.tableName} ").get(HNil)
    })

  implicit def updateQuery[TableFields <: HList, Columns <: HList, P <: HList, Q <: HList, R <: HList, Where <: Filter]
      (implicit
       placeholders: QueryFragmentBuilder[FT_ColumnNameAndPlaceholderList, Columns, P],
       where: QueryFragmentBuilder[FT_Where, Where, Q],
       concat: Concat.Aux[P, Q, R]
      ): QueryBuilder[UpdateQuery[TableFields, Columns, Where], R, HNil] =
    QueryBuilder.instance(update => {
      (placeholders.build(update.$fields).prepend("UPDATE " + update.table.tableName + " SET ") ++
        where.build(update.$where).prepend(" WHERE ")).get(HNil)
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

sealed trait QueryFragmentBuilder[TYPE, -X, P <: HList] {
  def build(x: X): CompiledQueryFragment[P]
}

object FragmentType {
  type FT_FieldExprAndAliasList = "FieldExprAndAliasList"
  type FT_FieldExprList = "FieldExprList"
  type FT_From = "From"
  type FT_Where = "FT_Where"
  type FT_ColumnNameList = "ColumnNameList"
  type FT_PlaceholderList = "PlaceholderList"
  type FT_ColumnNameAndPlaceholderList = "ColumnNameAndPlaceholderList"
  type FT_SortBy = "SortByList"
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

  implicit def fromSubQuery[SubQueryFields <: HList, S <: SelectQuery[_, _, _, _, _, _], P <: HList]
      (implicit query: QueryBuilder[S, P, _]): QueryFragmentBuilder[FT_From, SelectSubQuery[SubQueryFields, S] with Tagging[_], P] =
    instance(subQuery =>
      CompiledQueryFragment(query.build(subQuery.select))
        .wrap("(", ") AS " + subQuery.tagValue)
    )

  // ------------------------------
  // Src and alias lists
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
  // Src lists
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

  implicit def sortByField[S <: SortBy[_]]: QueryFragmentBuilder[FT_SortBy, S, HNil] =
    QueryFragmentBuilder.instance(sortBy => CompiledQueryFragment(sortBy.alias + " " + (sortBy.sortOrder match {
      case AscendingOrder => "ASC"
      case DescendingOrder => "DESC"
    })))

  implicit def sortByEmptyList: QueryFragmentBuilder[FT_SortBy, HNil, HNil] =
    QueryFragmentBuilder.instance(_ => CompiledQueryFragment.empty)

  implicit def sortByNonEmptyList[H, T <: HList]
      (implicit
       head: QueryFragmentBuilder[FT_SortBy, H, HNil],
       tail: QueryFragmentBuilder[FT_SortBy, T, HNil]): QueryFragmentBuilder[FT_SortBy, H :: T, HNil] =
    instance(clauses => head.build(clauses.head) `+,+` tail.build(clauses.tail))

  // ------------------------------
  // Field (src)
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
  // Column name and placeholder
  // ------------------------------

  implicit val columnName: QueryFragmentBuilder[FT_ColumnNameList, Column[_], HNil] =
    instance((f: Column[_]) => CompiledQueryFragment(f.name))

  implicit def emptyColumnNames: QueryFragmentBuilder[FT_ColumnNameList, HNil, HNil] =
    QueryFragmentBuilder.instance(_ => CompiledQueryFragment.empty)

  implicit def nonEmptyColumnNames[H, T <: HList, P <: HList, Q <: HList, R <: HList]
      (implicit
       head: QueryFragmentBuilder[FT_ColumnNameList, H, P],
       tail: QueryFragmentBuilder[FT_ColumnNameList, T, Q],
       concat: Concat.Aux[P, Q, R]): QueryFragmentBuilder[FT_ColumnNameList, H :: T, R] =
    instance { sources =>
      head.build(sources.head) `+,+` tail.build(sources.tail)
    }

  implicit def columnPlaceholder[A <: String with Singleton, T, PL <: HList]
      (implicit
       tag: ValueOf[A],
       fieldDecoder: FieldDecoder[T],
       fieldEncoder: FieldEncoder[T],
       queryFragmentBuilder: QueryFragmentBuilder[FT_FieldExprList, NamedPlaceholder[T] with Tagging[A], PL]
      ): QueryFragmentBuilder[FT_PlaceholderList, Column[T] with Tagging[A], PL] =
    instance((_: Column[T] with Tagging[A]) => queryFragmentBuilder.build(NamedPlaceholder[T, A]))

  implicit def emptyColumnPlaceholders: QueryFragmentBuilder[FT_PlaceholderList, HNil, HNil] =
    QueryFragmentBuilder.instance(_ => CompiledQueryFragment.empty)

  implicit def nonEmptyColumnPlaceholders[H, T <: HList, P <: HList, Q <: HList, R <: HList]
      (implicit
       head: QueryFragmentBuilder[FT_PlaceholderList, H, P],
       tail: QueryFragmentBuilder[FT_PlaceholderList, T, Q],
       concat: Concat.Aux[P, Q, R]): QueryFragmentBuilder[FT_PlaceholderList, H :: T, R] =
    instance { sources =>
      head.build(sources.head) `+,+` tail.build(sources.tail)
    }

  implicit def columnNameAndPlaceholder[A <: String with Singleton, T, PL <: HList]
      (implicit
       tag: ValueOf[A],
       fieldDecoder: FieldDecoder[T],
       fieldEncoder: FieldEncoder[T],
       queryFragmentBuilder: QueryFragmentBuilder[FT_FieldExprList, NamedPlaceholder[T] with Tagging[A], PL]
      ): QueryFragmentBuilder[FT_ColumnNameAndPlaceholderList, Column[T] with Tagging[A], PL] =
    instance((_: Column[T] with Tagging[A]) =>
      queryFragmentBuilder.build(NamedPlaceholder[T, A])
        .prepend(tag.value + " = "))

  implicit def emptyColumnNameAndPlaceholders: QueryFragmentBuilder[FT_ColumnNameAndPlaceholderList, HNil, HNil] =
    QueryFragmentBuilder.instance(_ => CompiledQueryFragment.empty)

  implicit def nonEmptyColumnNameAndPlaceholders[H, T <: HList, P <: HList, Q <: HList, R <: HList]
      (implicit
       head: QueryFragmentBuilder[FT_ColumnNameAndPlaceholderList, H, P],
       tail: QueryFragmentBuilder[FT_ColumnNameAndPlaceholderList, T, Q],
       concat: Concat.Aux[P, Q, R]): QueryFragmentBuilder[FT_ColumnNameAndPlaceholderList, H :: T, R] =
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
        case _: IsSubset[_, _] => e1.concatenateRequired(e2, " <@ ")
        case _: IsSuperset[_, _] => e1.concatenateRequired(e2, " @> ")
        case _: Overlaps[_, _] => e1.concatenateRequired(e2, " && ")
        case _: IsIncluded[_, _] => e1.concatenateRequired(e2.wrap("(", ")"), " = ANY")
        case _: InSubquery[_, _, _, _, _, _, _] => e1.concatenateRequired(e2.wrap("(", ")"), " IN ")
      }
    }

  implicit def whereField[P <: HList, F <: Field[_]](implicit field: QueryFragmentBuilder[FT_FieldExprList, F, P]): QueryFragmentBuilder[FT_Where, F, P] =
    instance(field.build)

  implicit def whereSubQuery[P <: HList, S <: SelectQuery[_, _, _, _, _, _]](implicit subQuery: QueryBuilder[S, P, _]): QueryFragmentBuilder[FT_Where, S, P] =
    instance(s => CompiledQueryFragment(subQuery.build(s)))
}
