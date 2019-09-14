package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields.{AlwaysTrue, And, Equals, GreaterThan, GreaterThanOrEqual, IsDefined, IsIncluded, IsNotDefined, IsSubset, IsSuperset, LessThan, LessThanOrEqual, Like, NotEquals, Or, Overlaps}
import io.epifab.yadl.typesafe.fields._
import shapeless.{::, HList, HNil, Lazy}

sealed trait QueryBuilder[-X, TYPE] {
  def build(x: X): Option[String]
}

object QueryBuilder {
  def apply[X](x: X)(implicit queryBuilder: QueryBuilder[X, "query"]): Option[String] =
    queryBuilder.build(x)

  def instance[T, U <: String](f: T => Option[String]): QueryBuilder[T, U] =
    new QueryBuilder[T, U] {
      override def build(x: T): Option[String] = f(x)
    }

  implicit private class ExtendedList[T](list: Iterable[T]) {
    def mapNonEmpty[U](f: Iterable[T] => U): Option[U] =
      if (list.isEmpty) None
      else Some(f(list))
  }

  implicit def query[FIELDS <: HList, GROUPBY <: HList, SOURCES <: HList]
    (implicit
     fields: QueryBuilder[FIELDS, "fields"],
     groupBy: QueryBuilder[GROUPBY, "groupBy"],
     from: QueryBuilder[SOURCES, "from"]): QueryBuilder[Select[_, FIELDS, GROUPBY, SOURCES], "query"] =
    QueryBuilder.instance(select =>
      Some(
        "SELECT " +
          Seq(
            fields.build(select.fields).orElse(Some("1")),
            from.build(select.sources).map("FROM " + _),
            groupBy.build(select.groupByFields).map("GROUP BY " + _)
          ).flatten.mkString(" ")
      )
    )

  implicit def emptySourceFrom: QueryBuilder[HNil, "from"] =
    QueryBuilder.instance(_ => None)

  implicit def nonEmptySourceFrom[H, T <: HList](implicit head: QueryBuilder[H, "from"], tail: QueryBuilder[T, "from"]): QueryBuilder[H :: T, "from"] =
    QueryBuilder.instance { sources =>
      Seq(
        head.build(sources.head),
        tail.build(sources.tail)
      ).flatten.reduceOption(_ + " " + _)
    }

  implicit def joinFrom[DS <: DataSource[_] with Tag[_]](implicit src: QueryBuilder[DS, "from"]): QueryBuilder[Join[DS], "from"] =
    QueryBuilder.instance(join => src.build(join.dataSource)
      .map("INNER JOIN " + _)
      .map(joinQuery =>
        where(join.filter)
          .map(joinQuery + " ON " + _)
          .getOrElse(joinQuery))
    )

  implicit def tableFrom: QueryBuilder[Table[_, _] with Tag[_], "from"] =
    QueryBuilder.instance(table => Some(table.tableName + " AS " + table.tagValue))

  implicit def subQueryFrom[SUBQUERYFIELDS <: HList, S <: Select[_, _, _, _]]
    (implicit query: QueryBuilder[S, "query"]): QueryBuilder[SubQuery[SUBQUERYFIELDS, S] with Tag[_], "from"] =
    QueryBuilder.instance(subQuery => query.build(subQuery.select).map(sql => "(" + sql + ") AS " + subQuery.tagValue))

  implicit def field: QueryBuilder[Field[_] with Tag[_], "fields"] =
    QueryBuilder.instance(field => Some(fieldFragment(field) + " AS " + field.tagValue))

  implicit def emptyFields: QueryBuilder[HNil, "fields"] =
    QueryBuilder.instance(_ => None)

  implicit def nonEmptyFields[H, T <: HList](implicit head: QueryBuilder[H, "fields"], tail: QueryBuilder[T, "fields"]): QueryBuilder[H :: T, "fields"] =
    QueryBuilder.instance(fields =>
      Seq(
        head.build(fields.head),
        tail.build(fields.tail)
      ).flatten.reduceOption(_ + ", " + _)
    )

  implicit def groupByField: QueryBuilder[Field[_], "groupBy"] = {
    QueryBuilder.instance(field => Some(fieldFragment(field)))
  }

  implicit def emptyGroupByFields: QueryBuilder[HNil, "groupBy"] =
    QueryBuilder.instance(_ => None)

  implicit def nonEmptyGroupByFields[H, T <: HList](implicit head: QueryBuilder[H, "groupBy"], tail: QueryBuilder[T, "groupBy"]): QueryBuilder[H :: T, "groupBy"] =
    QueryBuilder.instance(fields =>
      Seq(
        head.build(fields.head),
        tail.build(fields.tail)
      ).flatten.reduceOption(_ + ", " + _)
    )

  def where(filter: BinaryExpr): Option[String] = {
    filter match {
      case AlwaysTrue => None
      case filter => Some(binaryExprFragment(filter))
    }
  }

  private def fieldFragment(field: Field[_]): String = field match {
    case Column(name, srcAlias) => s"$srcAlias.$name"
    case cast@Cast(field) => fieldFragment(field) + "::" + cast.decoder.dbType.sqlName
    case Aggregation(field, dbFunction) => dbFunction.name + "(" + fieldFragment(field) + ")"
    case FieldExpr1(field, dbFunction) => dbFunction.name + "(" + fieldFragment(field) + ")"
    case FieldExpr2(field1, field2, dbFunction) => dbFunction.name + "(" + fieldFragment(field1) + "," + fieldFragment(field2) + ")"
    case FieldExpr3(field1, field2, field3, dbFunction) => dbFunction.name + "(" + fieldFragment(field1) + "," + fieldFragment(field2) + "," + fieldFragment(field3) + ")"
    case Placeholder(name) => s":$name"
  }

  private def binaryExprFragment(binaryExpr: BinaryExpr): String = binaryExpr match {
    case AlwaysTrue => "1 = 1"
    case And(left, right) => binaryExprFragment(left) + " AND " + binaryExprFragment(right)
    case Or(left, op) => "(" + binaryExprFragment(left) + " OR " + binaryExprFragment(op) + ")"
    case Equals(term1, term2) => fieldFragment(term1) + " = " + fieldFragment(term2)
    case NotEquals(term1, term2) => fieldFragment(term1) + " != " + fieldFragment(term2)
    case GreaterThan(term1, term2) => fieldFragment(term1) + " > " + fieldFragment(term2)
    case LessThan(term1, term2) => fieldFragment(term1) + " < " + fieldFragment(term2)
    case GreaterThanOrEqual(term1, term2) => fieldFragment(term1) + " >= " + fieldFragment(term2)
    case LessThanOrEqual(term1, term2) => fieldFragment(term1) + " <= " + fieldFragment(term2)
    case Like(term1, term2) => fieldFragment(term1) + " ILIKE " + fieldFragment(term2)
    case IsDefined(term) => fieldFragment(term) + " IS NOT NULL"
    case IsNotDefined(term) => fieldFragment(term) + " IS NULL"
    case IsSuperset(terms1, terms2) => fieldFragment(terms1) + " @> " + fieldFragment(terms2)
    case IsSubset(terms1, terms2) => fieldFragment(terms1) + " <@ " + fieldFragment(terms2)
    case Overlaps(terms1, terms2) => fieldFragment(terms1) + " && " + fieldFragment(terms2)
    case IsIncluded(term, terms) => fieldFragment(term) + " = ANY(" + fieldFragment(terms) + ")"
  }
}
