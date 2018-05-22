import domain._


object PostgresQueryBuilders {
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
  }

  val filterClauseBuilder: QueryBuilder[Filter.Expression.Clause[_]] = {
    case f: Filter.Expression.Clause.Field[_] => Query(f.field.src)
    case v: Filter.Expression.Clause.Value[_] => Query("?", Seq(v.value))
  }

  val filterExpressionBuilder: QueryBuilder[Filter.Expression] = {
    case Filter.Expression(left, right, op) =>
      filterClauseBuilder(left) ++ filterOpBuilder(op) ++ filterClauseBuilder(right)
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

  val select: QueryBuilder[SelectQuery] =
    (t: SelectQuery) =>
      Query("SELECT") ++
        t.fields
          .map(fieldBuilder.apply)
          .reduceOption(_ + "," ++ _)
          .getOrElse(Query("1")) ++
        Query("FROM") ++
          t.joins
            .foldLeft(dataSourceBuilder(t.dataSource))((from, join) => from ++ joinBuilder(join)) ++
        Query("WHERE") ++
          filterBuilder(t.filter)
}
