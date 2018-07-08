package io.epifab.yadl.postgres

import java.time.{LocalDate, LocalDateTime}

import io.epifab.yadl.domain.{Value, _}


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
    case Filter.Expression.Op.IsDefined => Query("IS NOT NULL")
    case Filter.Expression.Op.IsNotDefined => Query("IS NULL")
    case Filter.Expression.Op.Contains => Query("@>")
    case Filter.Expression.Op.Overlaps => Query("&&")
  }

  def valueBuilder: QueryBuilder[Value[_]] =
    (value: Value[_]) => {
      def toPlaceholder[T](t: T): String = t match {
        case Some(x) => toPlaceholder(x)
        case Json(x) => "cast(? as json)"
        case _: LocalDate => "cast(? as date)"
        case _: LocalDateTime => "cast(? as timestamp without time zone)"
        case _ => "?"
      }
      Query(toPlaceholder(value.value), Seq(value))
    }

  val filterClauseBuilder: QueryBuilder[Filter.Expression.Clause[_]] = {
    case Filter.Expression.Clause.Field(field) =>
      Query(field.src)

    case Filter.Expression.Clause.Literal(value) =>
      valueBuilder.apply(value)

    case Filter.Expression.Clause.AnyLiteral(values) =>
      Query("ANY(?)", Seq(values))
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
        (t.fields ++ t.aggregations)
          .map(fieldBuilder.apply)
          .reduceOption(_ + "," ++ _)
          .getOrElse(Query("1")) ++
        Query("FROM") ++
          t.joins
            .foldLeft(dataSourceBuilder(t.dataSource))((from, join) => from ++ joinBuilder(join)) ++
        Query("WHERE") ++
          filterBuilder(t.filter) ++
        t.aggregations
          .headOption
          .flatMap(_ =>
            t.fields.map(field => Query(field.src))
            .reduceOption(_ + "," ++ _)
            .map(fields => Query("GROUP BY") ++ fields)
          ) ++
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
        t.values
          .map(colValue => Query(colValue.column.name))
          .reduce(_ + "," ++ _)
          .wrap("(", ")") ++
      Query("VALUES") ++
        t.values
          .map(valueBuilder.apply)
          .reduce(_ + ", " + _)
          .wrap("(", ")")

  val update: QueryBuilder[Update] =
    (t: Update) =>
      Query("UPDATE") ++
        dataSourceBuilder(t.dataSource) ++
      Query("SET") ++
        t.values
          .map(colValue => Query(colValue.column.name) ++ Query("=") ++ valueBuilder(colValue))
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
