package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields._
import shapeless.{::, HList, HNil}

sealed trait QueryBuilder[-X] {
  def build(x: X): String
}

object QueryBuilder {
  def apply[X](x: X)(implicit queryBuilder: QueryBuilder[X]): String =
    queryBuilder.build(x)

  def instance[T](f: T => String): QueryBuilder[T] =
    new QueryBuilder[T] {
      override def build(x: T): String = f(x)
    }

  implicit def selectQuery[FIELDS <: HList, GROUPBY <: HList, SOURCES <: HList]
    (implicit
     fields: QueryFragmentBuilder[FIELDS, "fields"],
     groupBy: QueryFragmentBuilder[GROUPBY, "groupBy"],
     from: QueryFragmentBuilder[SOURCES, "from"]): QueryBuilder[Select[FIELDS, GROUPBY, SOURCES]] =
    QueryBuilder.instance(select =>
      "SELECT " +
        Seq(
          fields.build(select.fields).orElse(Some("1")),
          from.build(select.sources).map("FROM " + _),
          groupBy.build(select.groupByFields).map("GROUP BY " + _),
          QueryFragmentBuilder.binaryExprFragment(select.filter).map("WHERE " + _)
        ).flatten.mkString(" ")
    )
}

sealed trait QueryFragmentBuilder[-X, TYPE] {
  def build(x: X): Option[String]
}

object QueryFragmentBuilder {
  def apply[X](x: X)(implicit queryFragmentBuilder: QueryFragmentBuilder[X, "query"]): Option[String] =
    queryFragmentBuilder.build(x)

  def instance[T, U <: String](f: T => Option[String]): QueryFragmentBuilder[T, U] =
    new QueryFragmentBuilder[T, U] {
      override def build(x: T): Option[String] = f(x)
    }

  implicit private class ExtendedList[T](list: Iterable[T]) {
    def mapNonEmpty[U](f: Iterable[T] => U): Option[U] =
      if (list.isEmpty) None
      else Some(f(list))
  }

  implicit def emptySourceFrom: QueryFragmentBuilder[HNil, "from"] =
    QueryFragmentBuilder.instance(_ => None)

  implicit def nonEmptySourceFrom[H, T <: HList](implicit head: QueryFragmentBuilder[H, "from"], tail: QueryFragmentBuilder[T, "from"]): QueryFragmentBuilder[H :: T, "from"] =
    QueryFragmentBuilder.instance { sources =>
      Seq(
        head.build(sources.head),
        tail.build(sources.tail)
      ).flatten.reduceOption(_ + " " + _)
    }

  implicit def joinFrom[DS <: DataSource[_] with Tag[_]](implicit src: QueryFragmentBuilder[DS, "from"]): QueryFragmentBuilder[Join[DS], "from"] =
    QueryFragmentBuilder.instance(join => src.build(join.dataSource)
      .map("INNER JOIN " + _)
      .map(joinQuery =>
        binaryExprFragment(join.filter)
          .map(joinQuery + " ON " + _)
          .getOrElse(joinQuery))
    )

  implicit def tableFrom: QueryFragmentBuilder[Table[_, _] with Tag[_], "from"] =
    QueryFragmentBuilder.instance(table => Some(table.tableName + " AS " + table.tagValue))

  implicit def subQueryFrom[SUBQUERYFIELDS <: HList, S <: Select[_, _, _]]
    (implicit query: QueryBuilder[S]): QueryFragmentBuilder[SubQuery[SUBQUERYFIELDS, S] with Tag[_], "from"] =
    QueryFragmentBuilder.instance(subQuery => Some("(" + query.build(subQuery.select) + ") AS " + subQuery.tagValue))

  implicit def field: QueryFragmentBuilder[Field[_] with Tag[_], "fields"] =
    QueryFragmentBuilder.instance(field => Some(fieldFragment(field) + " AS " + field.tagValue))

  implicit def emptyFields: QueryFragmentBuilder[HNil, "fields"] =
    QueryFragmentBuilder.instance(_ => None)

  implicit def nonEmptyFields[H, T <: HList](implicit head: QueryFragmentBuilder[H, "fields"], tail: QueryFragmentBuilder[T, "fields"]): QueryFragmentBuilder[H :: T, "fields"] =
    QueryFragmentBuilder.instance(fields =>
      Seq(
        head.build(fields.head),
        tail.build(fields.tail)
      ).flatten.reduceOption(_ + ", " + _)
    )

  implicit def groupByField: QueryFragmentBuilder[Field[_], "groupBy"] = {
    QueryFragmentBuilder.instance(field => Some(fieldFragment(field)))
  }

  implicit def emptyGroupByFields: QueryFragmentBuilder[HNil, "groupBy"] =
    QueryFragmentBuilder.instance(_ => None)

  implicit def nonEmptyGroupByFields[H, T <: HList](implicit head: QueryFragmentBuilder[H, "groupBy"], tail: QueryFragmentBuilder[T, "groupBy"]): QueryFragmentBuilder[H :: T, "groupBy"] =
    QueryFragmentBuilder.instance(fields =>
      Seq(
        head.build(fields.head),
        tail.build(fields.tail)
      ).flatten.reduceOption(_ + ", " + _)
    )

  def fieldFragment(field: Field[_]): String = field match {
    case Column(name, srcAlias) => s"$srcAlias.$name"
    case cast@Cast(field) => fieldFragment(field) + "::" + cast.decoder.dbType.sqlName
    case Aggregation(field, dbFunction) => dbFunction.name + "(" + fieldFragment(field) + ")"
    case FieldExpr1(field, dbFunction) => dbFunction.name + "(" + fieldFragment(field) + ")"
    case FieldExpr2(field1, field2, dbFunction) => dbFunction.name + "(" + fieldFragment(field1) + "," + fieldFragment(field2) + ")"
    case FieldExpr3(field1, field2, field3, dbFunction) => dbFunction.name + "(" + fieldFragment(field1) + "," + fieldFragment(field2) + "," + fieldFragment(field3) + ")"
    case Placeholder(name) => s":$name"
  }

  def binaryExprFragment(binaryExpr: BinaryExpr): Option[String] = binaryExpr match {
    case AlwaysTrue => None
    case And(left, right) => (binaryExprFragment(left), binaryExprFragment(right)) match {
      case (Some(l), Some(r)) => Some(l + " AND " + r)
      case (Some(l), None) => Some(l)
      case (None, Some(r)) => Some(r)
      case (None, None) => None
    }
    case Or(left, right) => (binaryExprFragment(left), binaryExprFragment(right)) match {
      case (Some(l), Some(r)) => Some("(" + l + " OR " + r + ")")
      case (Some(l), None) => Some(l)
      case (None, Some(r)) => Some(r)
      case (None, None) => None
    }
    case Equals(term1, term2) => Some(fieldFragment(term1) + " = " + fieldFragment(term2))
    case NotEquals(term1, term2) => Some(fieldFragment(term1) + " != " + fieldFragment(term2))
    case GreaterThan(term1, term2) => Some(fieldFragment(term1) + " > " + fieldFragment(term2))
    case LessThan(term1, term2) => Some(fieldFragment(term1) + " < " + fieldFragment(term2))
    case GreaterThanOrEqual(term1, term2) => Some(fieldFragment(term1) + " >= " + fieldFragment(term2))
    case LessThanOrEqual(term1, term2) => Some(fieldFragment(term1) + " <= " + fieldFragment(term2))
    case Like(term1, term2) => Some(fieldFragment(term1) + " ILIKE " + fieldFragment(term2))
    case IsDefined(term) => Some(fieldFragment(term) + " IS NOT NULL")
    case IsNotDefined(term) => Some(fieldFragment(term) + " IS NULL")
    case IsSuperset(terms1, terms2) => Some(fieldFragment(terms1) + " @> " + fieldFragment(terms2))
    case IsSubset(terms1, terms2) => Some(fieldFragment(terms1) + " <@ " + fieldFragment(terms2))
    case Overlaps(terms1, terms2) => Some(fieldFragment(terms1) + " && " + fieldFragment(terms2))
    case IsIncluded(term, terms) => Some(fieldFragment(term) + " = ANY(" + fieldFragment(terms) + ")")
  }
}
