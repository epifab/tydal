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

  val columnSrcQueryBuilder: QueryBuilder[Column[_]] = {
    case TableColumn(name, table) =>
      s"${table.tableAlias}.$name"

    case AggregateColumn(column, aggregateFunction) =>
      Query(aggregateFunction.name) + "(" + columnSrcQueryBuilder(column) + ")"
  }

  val columnAliasQueryBuilder: QueryBuilder[Column[_]] = {
    case TableColumn(name, table) =>
      s"${table.tableAlias}__$name"

    case AggregateColumn(column, aggregateFunction) =>
      Query(aggregateFunction.name) + "_" + columnAliasQueryBuilder(column)
  }

  val filterClauseBuilder: QueryBuilder[Filter.Expression.Clause[_]] = {
    case Filter.Expression.Clause.Column(column) =>
      columnSrcQueryBuilder(column)

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

  val columnBuilder: QueryBuilder[Column[_]] =
    (column: Column[_]) => columnSrcQueryBuilder(column) ++ "AS" ++ columnAliasQueryBuilder(column)

  val tableWithAliasBuilder: QueryBuilder[Table] =
    (table: Table) => Query(s"${table.tableName} AS ${table.tableAlias}")

  val joinBuilder: QueryBuilder[Join] = {
    case InnerJoin(source, clauses) =>
      Query("INNER JOIN") ++ tableWithAliasBuilder(source) ++ "ON" ++ filterBuilder(clauses)
    case LeftJoin(source, clauses) =>
      Query("LEFT JOIN") ++ tableWithAliasBuilder(source) ++ "ON" ++ filterBuilder(clauses)
    case CrossJoin(source) =>
      Query("CROSS JOIN") ++ tableWithAliasBuilder(source)
  }

  val sortBuilder: QueryBuilder[Sort] =
    (s: Sort) => {
      columnSrcQueryBuilder(s.column) ++ (s match {
        case _: AscSort => "ASC"
        case _: DescSort => "DESC"
      })
    }

  val select: QueryBuilder[Select] =
    (t: Select) =>
      Query("SELECT") ++
        (t.columns ++ t.aggregations)
          .map(columnBuilder.apply)
          .reduceOption(_ + "," ++ _)
          .getOrElse(Query("1")) ++
        Query("FROM") ++
          t.joins
            .foldLeft(tableWithAliasBuilder(t.table))((from, join) => from ++ joinBuilder(join)) ++
        Query("WHERE") ++
          filterBuilder(t.filter) ++
        t.aggregations
          .headOption
          .flatMap(_ =>
            t.columns.map(columnSrcQueryBuilder.apply)
            .reduceOption(_ + "," ++ _)
            .map(columns => Query("GROUP BY") ++ columns)
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
        t.table.tableName ++
        t.columnValues
          .map(colValue => Query(colValue.column.name))
          .reduce(_ + "," ++ _)
          .wrap("(", ")") ++
      Query("VALUES") ++
        t.columnValues
          .map(columnValue => valueBuilder(columnValue.value))
          .reduce(_ + ", " + _)
          .wrap("(", ")")

  val update: QueryBuilder[Update] =
    (t: Update) =>
      Query("UPDATE") ++
        tableWithAliasBuilder(t.table) ++
      Query("SET") ++
        t.values
          .map(colValue => Query(colValue.column.name) ++ Query("=") ++ valueBuilder(colValue.value))
          .reduce(_ + "," ++ _) ++
      Query("WHERE") ++
        filterBuilder(t.filter)

  val delete: QueryBuilder[Delete] =
    (t: Delete) =>
      Query("DELETE FROM") ++
        tableWithAliasBuilder(t.table) ++
      Query("WHERE") ++
        filterBuilder(t.filter)

  val build: QueryBuilder[Statement] = {
    case s: Select => select(s)
    case s: Insert => insert(s)
    case s: Update => update(s)
    case s: Delete => delete(s)
  }
}
