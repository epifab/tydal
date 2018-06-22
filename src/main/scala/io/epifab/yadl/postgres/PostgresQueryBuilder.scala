package io.epifab.yadl.postgres

import io.epifab.yadl.domain._


object PostgresQueryBuilder {
  implicit private val fromString: String => Query = Query(_)

  val filterOpBuilder: QueryBuilder[Filter.Expression.Op] = {
    case Filter.Expression.Op.Equal => Query("=")
    case Filter.Expression.Op.GT => Query(">")
    case Filter.Expression.Op.LT => Query("<")
    case Filter.Expression.Op.GTE => Query(">=")
    case Filter.Expression.Op.LTE => Query("<=")
    case Filter.Expression.Op.NotEqual => Query("<>")
    case Filter.Expression.Op.Like => Query("LIKE")
    case Filter.Expression.Op.In => Query("IN")
    case Filter.Expression.Op.IsDefined => Query("IS NOT NULL")
    case Filter.Expression.Op.IsNotDefined => Query("IS NULL")
  }

  val filterClauseBuilder: QueryBuilder[Filter.Expression.Clause[_]] = {
    case f: Filter.Expression.Clause.Field[_] =>
      Query(f.field.src)
    case l: Filter.Expression.Clause.Literal[_, _] =>
      Query("?", Seq(l))
//      literal.dbValue match {
//        // todo: there must be a way to inject a sequence
//        case iterable: Iterable[_] =>
//          iterable
//            .map(element => Query("?", Seq(element)))
//            .reduceOption(_ + "," ++ _)
//            .getOrElse(Query.empty)
//            .wrap("(", ")")
//        case any =>
//          Query("?", Seq(any))
//      }
  }

  val filterExpressionBuilder: QueryBuilder[Filter.Expression] = {
    case Filter.BinaryExpression(left, right, op) =>
      filterClauseBuilder(left) ++ filterOpBuilder(op) ++ filterClauseBuilder(right)
    case Filter.UniaryExpression(left, op) =>
      filterClauseBuilder(left) ++ filterOpBuilder(op)
  }

  val filterBuilder: QueryBuilder[Filter] = {
    case e: Filter.Expression => filterExpressionBuilder(e)
    case Filter.And(f1, f2) => filterBuilder(f1) ++ "AND" ++ filterBuilder(f2)
    case Filter.Or(f1, f2) => Query("(") + filterBuilder(f1) ++ "OR" ++ filterBuilder(f2) + ")"
    case Filter.Empty => Query("1 = 1")
  }

  val fieldBuilder: QueryBuilder[Field[_]] =
    (field: Field[_]) => Query(s"${field.src} AS ${field.alias}")

  val dataSourceBuilder: QueryBuilder[DataSource] =
    (ds: DataSource) => Query(s"${ds.src} AS ${ds.alias}")

  val joinBuilder: QueryBuilder[Join] = {
    case InnerJoin(source, clauses) =>
      Query("INNER JOIN") ++ dataSourceBuilder(source) ++ "ON" ++ filterBuilder(clauses)
    case LeftJoin(source, clauses) =>
      Query("LEFT JOIN") ++ dataSourceBuilder(source) ++ "ON" ++ filterBuilder(clauses)
    case CrossJoin(source) =>
      Query("CROSS JOIN") ++ dataSourceBuilder(source)
  }

  val sortBuilder: QueryBuilder[Sort] =
    (s: Sort) => {
      Query(s.source.src) ++ (s match {
        case _: AscSort => "ASC"
        case _: DescSort => "DESC"
      })
    }

  val select: QueryBuilder[Select] =
    (t: Select) =>
      Query("SELECT") ++
        t.fields
          .map(fieldBuilder.apply)
          .reduceOption(_ + "," ++ _)
          .getOrElse(Query("1")) ++
        Query("FROM") ++
          t.joins
            .foldLeft(dataSourceBuilder(t.dataSource))((from, join) => from ++ joinBuilder(join)) ++
        Query("WHERE") ++
          filterBuilder(t.filter) ++
        t.sort
          .map(sortBuilder.apply)
          .reduceOption(_ + "," ++ _)
          .map(sort => Query("ORDER BY") ++ sort) ++
        t.limit.map(limit =>
          Query("OFFSET") ++ limit.start.toString ++
          Query("LIMIT") ++ limit.stop.toString)

  val insert: QueryBuilder[Insert] =
    (t: Insert) =>
      Query("INSERT INTO") ++
        t.dataSource.src ++
        t.fieldValues
          .map(fieldValue => Query(fieldValue.field.name))
          .reduce(_ + "," ++ _)
          .wrap("(", ")") ++
      Query("VALUES") ++
        t.fieldValues
          .map(fieldValue => Query("?", Seq(fieldValue)))
          .reduce(_ + ", " + _)
          .wrap("(", ")")

  val update: QueryBuilder[Update] =
    (t: Update) =>
      Query("UPDATE") ++
        dataSourceBuilder(t.dataSource) ++
      Query("SET") ++
        t.fieldValues
          .map(fieldValue => Query(s"${fieldValue.field.name} = ?", Seq(fieldValue)))
          .reduce(_ + "," ++ _) ++
      Query("WHERE") ++
        filterBuilder(t.filter)

  val delete: QueryBuilder[Delete] =
    (t: Delete) =>
      Query("DELETE FROM") ++
        dataSourceBuilder(t.dataSource) ++
      Query("WHERE") ++
        filterBuilder(t.filter)

  val build: QueryBuilder[Statement] = {
    case s: Select => select(s)
    case s: Insert => insert(s)
    case s: Update => update(s)
    case s: Delete => delete(s)
  }
}
