package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields._
import shapeless.{HList, HNil, ::}

sealed trait QueryBuilder[-X, TYPE <: String] {
  def build(x: X): Option[String]
}

object QueryBuilder {
//  def apply[X](x: X)(implicit queryBuilder: QueryBuilder[X, "query"]): Option[String] =
//    queryBuilder.build(x)

  def apply[X, TYPE <: String](x: X)(implicit queryBuilder: QueryBuilder[X, TYPE]): Option[String] =
    queryBuilder.build(x)

  def instance[T, U <: String](f: T => Option[String]): QueryBuilder[T, U] =
    new QueryBuilder[T, U] {
      override def build(x: T): Option[String] = f(x)
    }

  implicit class ExtendedList[T](list: Iterable[T]) {
    def mapNonEmpty[U](f: Iterable[T] => U): Option[U] =
      if (list.isEmpty) None
      else Some(f(list))
  }

  implicit def query[FIELDS <: HList, GROUPBY <: HList, SOURCES <: HList]
    (implicit
     fields: QueryBuilder[FIELDS, "fields"],
     groupBy: QueryBuilder[GROUPBY, "groupBy"],
     from: QueryBuilder[SOURCES, "from"]): QueryBuilder[Select[HList, FIELDS, GROUPBY, SOURCES], "query"] =
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

  implicit def nonEmptySourceFrom[H <: DataSource[_], T <: HList](implicit head: QueryBuilder[H, "from"], tail: QueryBuilder[T, "from"]): QueryBuilder[H :: T, "from"] =
    QueryBuilder.instance(sources => for {
      h <- head.build(sources.head)
      t <- tail.build(sources.tail)
    } yield (h + " " + t))

  implicit def tableFrom: QueryBuilder[Table[_, _] with Tag[_], "from"] =
    QueryBuilder.instance(table => Some(table.tableName + " AS " + table.tagValue))

  implicit def subQueryFrom[FIELDS <: HList, GROUPBY <: HList, SOURCES <: HList]
    (implicit query: QueryBuilder[Select[HList, FIELDS, GROUPBY, SOURCES], "query"]): QueryBuilder[SubQuery[HList, FIELDS, HList, GROUPBY, SOURCES] with Tag[_], "from"] =
    QueryBuilder.instance(subQuery => query.build(subQuery.select).map(sql => "(" + sql + ") AS " + subQuery.tagValue))

  implicit def fields[FIELDS](implicit tagMap: TagMap[Field[Any], FIELDS]): QueryBuilder[FIELDS, "fields"] =
    new QueryBuilder[FIELDS, "fields"] {
      def build(fields: FIELDS): Option[String] = {
        tagMap.toMap(fields)
          .map[String] { case (alias, field) => fieldSrc(field) + " AS " + alias }
          .mapNonEmpty(_.mkString(", "))
      }

      def fieldSrc(field: Field[_]): String = field match {
        case Column(name, srcAlias) => s"$srcAlias.$name"
        case cast@ Cast(field) => fieldSrc(field) + "::" + cast.decoder.dbType.sqlName
        case Aggregation(field, dbFunction) => dbFunction.name + "(" + fieldSrc(field) + ")"
        case FieldExpr1(field, dbFunction) => dbFunction.name + "(" + fieldSrc(field) + ")"
        case FieldExpr2(field1, field2, dbFunction) => dbFunction.name + "(" + fieldSrc(field1) + "," + fieldSrc(field2) + ")"
        case FieldExpr3(field1, field2, field3, dbFunction) => dbFunction.name + "(" + fieldSrc(field1) + "," + fieldSrc(field2) + "," + fieldSrc(field3) + ")"
        case Placeholder(name) => s":$name"
      }
    }

  implicit def groupBy[FIELDS](implicit tagMap: TagMap[Field[Any], FIELDS]): QueryBuilder[FIELDS, "groupBy"] =
    QueryBuilder.instance(fields => tagMap.toMap(fields).keys.mapNonEmpty(_.mkString(", ")))
}
