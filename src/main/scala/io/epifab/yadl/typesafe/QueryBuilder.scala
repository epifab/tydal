package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields._
import shapeless.{::, HList, HNil}

case class Query(sql: String, parameterNames: Seq[Placeholder[_, _]]) {
  def ++(other: Query): Query =
    Query(sql + other.sql, parameterNames ++ other.parameterNames)

  def ++(other: String): Query =
    append(other)

  def append(right: String): Query =
    Query(sql + right, parameterNames)

  def prepend(left: String): Query =
    Query(left + sql, parameterNames)

  def wrap(left: String, right: String): Query =
    Query(left + sql + right, parameterNames)
}

object Query {
  def apply(sql: String): Query = Query(sql, Seq.empty)
}

case class QueryFragment(query: Option[Query]) {
  def ++(other: QueryFragment): QueryFragment =
    concatenate(other, "")

  def `+ +`(other: QueryFragment): QueryFragment =
    concatenate(other, " ")

  def `+,+`(other: QueryFragment): QueryFragment =
    concatenate(other, ", ")

  def `+AND+`(other: QueryFragment): QueryFragment =
    concatenate(other, " AND ")

  def `+OR+`(other: QueryFragment): QueryFragment =
    concatenate(other, " OR ")

  def map(f: String => String): QueryFragment =
    QueryFragment(query.map(q => Query(f(q.sql), q.parameterNames)))

  def flatMap(f: Query => QueryFragment): QueryFragment =
    query.map(f).getOrElse(this)

  def orElse(default: Query): QueryFragment =
    QueryFragment(query.getOrElse(default))

  def getOrElse(default: Query): Query =
    query.getOrElse(default)

  def concatenate(other: QueryFragment, separator: String): QueryFragment =
    QueryFragment(
      Seq(query, other.query)
        .flatten
        .reduceOption(_ ++ Query(separator) ++ _))
}

object QueryFragment {
  def unapply(arg: QueryFragment): Option[Option[Query]] =
    Some(arg.query)

  def empty: QueryFragment = QueryFragment(None)

  def apply(query: Query): QueryFragment =
    QueryFragment(Some(query))

  def apply(sql: String): QueryFragment =
    QueryFragment(Some(new Query(sql, Seq.empty)))

  def apply(sql: String, placeholder: Placeholder[_, _]): QueryFragment =
    QueryFragment(Some(new Query(sql, Seq(placeholder))))
}

sealed trait QueryBuilder[-X] {
  def build(x: X): Query
}

object QueryBuilder {
  def apply[X](x: X)(implicit queryBuilder: QueryBuilder[X]): Query =
    queryBuilder.build(x)

  def instance[T](f: T => Query): QueryBuilder[T] =
    new QueryBuilder[T] {
      override def build(x: T): Query = f(x)
    }

  implicit def selectQuery[FIELDS <: HList, GROUPBY <: HList, SOURCES <: HList]
    (implicit
     fields: QueryFragmentBuilder["fields", FIELDS],
     groupBy: QueryFragmentBuilder["groupBy", GROUPBY],
     from: QueryFragmentBuilder["from", SOURCES]): QueryBuilder[Select[FIELDS, GROUPBY, SOURCES]] =
    QueryBuilder.instance(select =>
      Query("SELECT ") ++
        (fields.build(select.fields) `+ +`
          from.build(select.sources).map("FROM " + _) `+ +`
          groupBy.build(select.groupByFields).map("GROUP BY " + _) `+ +`
          QueryFragmentBuilder.binaryExprFragment(select.filter).map("WHERE " + _)
        ).getOrElse(Query("1"))
    )
}

sealed trait QueryFragmentBuilder[TYPE, -X] {
  def build(x: X): QueryFragment
}

object QueryFragmentBuilder {
  def apply[X](x: X)(implicit queryFragmentBuilder: QueryFragmentBuilder["query", X]): QueryFragment =
    queryFragmentBuilder.build(x)

  def instance[T, U <: String](f: T => QueryFragment): QueryFragmentBuilder[U, T] =
    new QueryFragmentBuilder[U, T] {
      override def build(x: T): QueryFragment = f(x)
    }

  implicit private class ExtendedList[T](list: Iterable[T]) {
    def mapNonEmpty[U](f: Iterable[T] => U): Option[U] =
      if (list.isEmpty) None
      else Some(f(list))
  }

  implicit def emptySourceFrom: QueryFragmentBuilder["from", HNil] =
    QueryFragmentBuilder.instance(_ => QueryFragment.empty)

  implicit def nonEmptySourceFrom[H, T <: HList](implicit head: QueryFragmentBuilder["from", H], tail: QueryFragmentBuilder["from", T]): QueryFragmentBuilder["from", H :: T] =
    QueryFragmentBuilder.instance { sources =>
      head.build(sources.head) `+ +` tail.build(sources.tail)
    }

  implicit def joinFrom[DS <: DataSource[_] with Tag[_]](implicit src: QueryFragmentBuilder["from", DS]): QueryFragmentBuilder["from", Join[DS]] =
    QueryFragmentBuilder.instance(join =>
      src.build(join.dataSource).map((q: String) => "INNER JOIN " + q) ++
        binaryExprFragment(join.filter).map(" ON " + _)
    )

  implicit def tableFrom: QueryFragmentBuilder["from", Table[_, _] with Tag[_]] =
    QueryFragmentBuilder.instance(table => QueryFragment(table.tableName + " AS " + table.tagValue))

  implicit def subQueryFrom[SUBQUERYFIELDS <: HList, S <: Select[_, _, _]]
    (implicit query: QueryBuilder[S]): QueryFragmentBuilder["from", SubQuery[SUBQUERYFIELDS, S] with Tag[_]] =
    QueryFragmentBuilder.instance(subQuery =>
      QueryFragment(
        query.build(subQuery.select)
          .wrap("(", ") AS " + subQuery.tagValue)
      )
    )

  implicit def field: QueryFragmentBuilder["fields", Field[_] with Tag[_]] =
    QueryFragmentBuilder.instance(field => QueryFragment(fieldFragment(field) ++ " AS " ++ field.tagValue))

  implicit def emptyFields: QueryFragmentBuilder["fields", HNil] =
    QueryFragmentBuilder.instance(_ => QueryFragment.empty)

  implicit def nonEmptyFields[H, T <: HList](implicit head: QueryFragmentBuilder["fields", H], tail: QueryFragmentBuilder["fields", T]): QueryFragmentBuilder["fields", H :: T] =
    QueryFragmentBuilder.instance(fields =>
      head.build(fields.head) `+,+` tail.build(fields.tail)
    )

  implicit def groupByField: QueryFragmentBuilder["groupBy", Field[_]] = {
    QueryFragmentBuilder.instance(field => QueryFragment(fieldFragment(field)))
  }

  implicit def emptyGroupByFields: QueryFragmentBuilder["groupBy", HNil] =
    QueryFragmentBuilder.instance(_ => QueryFragment.empty)

  implicit def nonEmptyGroupByFields[H, T <: HList](implicit head: QueryFragmentBuilder["groupBy", H], tail: QueryFragmentBuilder["groupBy", T]): QueryFragmentBuilder["groupBy", H :: T] =
    QueryFragmentBuilder.instance(fields =>
      head.build(fields.head) `+,+` tail.build(fields.tail)
    )

  def fieldFragment(field: Field[_]): Query = field match {
    case Column(name, srcAlias) =>
      Query(s"$srcAlias.$name")

    case cast@Cast(field) =>
      fieldFragment(field).append("::" + cast.decoder.dbType.sqlName)

    case Aggregation(field, dbFunction) =>
      fieldFragment(field).wrap(dbFunction.name + "(", ")")

    case FieldExpr1(field, dbFunction) =>
      fieldFragment(field).wrap(dbFunction.name + "(", ")")

    case FieldExpr2(field1, field2, dbFunction) =>
      Query(dbFunction.name + "(") ++ fieldFragment(field1) ++ "," ++ fieldFragment(field2) ++ ")"

    case placeholder@ Placeholder(_) => new Query("?", Seq(placeholder))
  }

  def binaryExprFragment(binaryExpr: BinaryExpr): QueryFragment = binaryExpr match {
    case AlwaysTrue => QueryFragment.empty

    case And(left, right) =>
      binaryExprFragment(left) `+AND+` binaryExprFragment(right)

    case Or(left, right) =>
      binaryExprFragment(left) `+OR+` binaryExprFragment(right)

    case Equals(term1, term2) =>
      QueryFragment(fieldFragment(term1) ++ " = " ++ fieldFragment(term2))

    case NotEquals(term1, term2) =>
      QueryFragment(fieldFragment(term1) ++ " != " ++ fieldFragment(term2))

    case GreaterThan(term1, term2) =>
      QueryFragment(fieldFragment(term1) ++ " > " ++ fieldFragment(term2))

    case LessThan(term1, term2) =>
      QueryFragment(fieldFragment(term1) ++ " < " ++ fieldFragment(term2))

    case GreaterThanOrEqual(term1, term2) =>
      QueryFragment(fieldFragment(term1) ++ " >= " ++ fieldFragment(term2))

    case LessThanOrEqual(term1, term2) =>
      QueryFragment(fieldFragment(term1) ++ " <= " ++ fieldFragment(term2))

    case Like(term1, term2) =>
      QueryFragment(fieldFragment(term1) ++ " ILIKE " ++ fieldFragment(term2))

    case IsDefined(term) =>
      QueryFragment(fieldFragment(term) ++ " IS NOT NULL")

    case IsNotDefined(term) =>
      QueryFragment(fieldFragment(term) ++ " IS NULL")

    case IsSuperset(terms1, terms2) =>
      QueryFragment(fieldFragment(terms1) ++ " @> " ++ fieldFragment(terms2))

    case IsSubset(terms1, terms2) =>
      QueryFragment(fieldFragment(terms1) ++ " <@ " ++ fieldFragment(terms2))

    case Overlaps(terms1, terms2) =>
      QueryFragment(fieldFragment(terms1) ++ " && " ++ fieldFragment(terms2))

    case IsIncluded(term, terms) =>
      QueryFragment(fieldFragment(term) ++ " = ANY(" ++ fieldFragment(terms) ++ ")")
  }
}
