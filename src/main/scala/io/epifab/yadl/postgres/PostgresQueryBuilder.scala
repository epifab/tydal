package io.epifab.yadl.postgres


import io.epifab.yadl.domain._


class AliasLookup(prefix: String) {
  private var count: Int = 0

  private val aliases: scala.collection.mutable.Map[Any, String] =
    new scala.collection.mutable.HashMap

  private def nextAlias: String = synchronized {
    count += 1
    prefix + count.toString
  }

  def apply(t: Any): String =
    aliases.getOrElseUpdate(t, nextAlias)
}


class PostgresQueryBuilder(aliasLookup: AliasLookup) {
  def filterOpBuilder: QueryBuilder[Filter.Expression.Op] = {
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
      def toPlaceholder[T](dbType: DbType[T]): String = dbType match {
        case OptionDbType(innerDbType) => toPlaceholder(innerDbType)
        case JsonDbType => "cast(? as json)"
        case EnumDbType(name) => s"cast(? as $name)"
        case DateDbType => "cast(? as date)"
        case DateTimeDbType => "cast(? as timestamp without time zone)"
        case EnumSeqDbType(enum) => s"cast(? as ${enum.name}[])"
        case _ => "?"
      }

      Query(toPlaceholder(value.adapter.dbType), Seq(value))
    }

  def termSrcQueryBuilder: QueryBuilder[Term[_]] = {
    case Column(name, table) =>
      Query(aliasLookup(table) + "." + name)

    case Aggregation(term, aggregateFunction) =>
      Query(aggregateFunction.name) :+ "(" :+ termSrcQueryBuilder(term) :+ ")"

    case SubQueryTerm(term, subQuery) =>
      Query(aliasLookup(subQuery)) :+ "." :+ termAliasQueryBuilder(term)

    case value: Value[_] =>
      valueBuilder(value)
  }

  def termAliasQueryBuilder: QueryBuilder[Term[_]] = {
    case Column(name, table) =>
      Query(aliasLookup(table) + "__" + name)

    case Aggregation(term, aggregateFunction) =>
      Query(aggregateFunction.name) :+ "_" :+ termAliasQueryBuilder(term)

    case SubQueryTerm(term, subQuery) =>
      Query(aliasLookup(subQuery)) :+ "__" :+ termAliasQueryBuilder(term)

    case value: Value[_] =>
      Query(aliasLookup(value))
  }

  def filterClauseBuilder: QueryBuilder[Filter.Expression.Clause[_]] = {
    case Filter.Expression.Clause.Term(term) =>
      termSrcQueryBuilder(term)

    // TODO: Any should be moved to terms and made generic (unary expression)
    case Filter.Expression.Clause.AnyTerm(values) =>
      Query("ANY(?)", Seq(values))
  }

  def filterExpressionBuilder: QueryBuilder[Filter.Expression] = {
    case Filter.BinaryExpression(left, right, op) =>
      filterClauseBuilder(left) :++ filterOpBuilder(op) :++ filterClauseBuilder(right)
    case Filter.UniaryExpression(left, op) =>
      filterClauseBuilder(left) :++ filterOpBuilder(op)
  }

  def filterBuilder: QueryBuilder[Filter] = {
    case e: Filter.Expression => filterExpressionBuilder(e)
    case Filter.And(f1, f2) => filterBuilder(f1) :++ "AND" :++ filterBuilder(f2)
    case Filter.Or(f1, f2) => (filterBuilder(f1) :++ "OR" :++ filterBuilder(f2)).wrap("(", ")")
    case Filter.Empty => Query("1 = 1")
  }

  def termBuilder: QueryBuilder[Term[_]] =
    (term: Term[_]) => termSrcQueryBuilder(term) :++ "AS" :++ termAliasQueryBuilder(term)

  def dataSourceWithAliasBuilder: QueryBuilder[DataSource] = {
    case dataSource: Table[_] =>
      Query(dataSource.tableName + " AS " + aliasLookup(dataSource))
    case dataSource: TableProjection[_, _] =>
      Query(dataSource.table.tableName + " AS " + aliasLookup(dataSource.table))
    case dataSource: SubQuery[_, _] =>
      select(dataSource.select).wrap("(", ")") :++ "AS" :++ aliasLookup(dataSource)
  }

  def joinBuilder: QueryBuilder[Join] = {
    case InnerJoin(source, clauses) =>
      Query("INNER JOIN") :++ dataSourceWithAliasBuilder(source) :++ "ON" :++ filterBuilder(clauses)
    case LeftJoin(source, clauses) =>
      Query("LEFT JOIN") :++ dataSourceWithAliasBuilder(source) :++ "ON" :++ filterBuilder(clauses)
    case CrossJoin(source) =>
      Query("CROSS JOIN") :++ dataSourceWithAliasBuilder(source)
  }

  def sortBuilder: QueryBuilder[Sort] =
    (s: Sort) => {
      termSrcQueryBuilder(s.term) :++ (s match {
        case _: AscSort => Query("ASC")
        case _: DescSort => Query("DESC")
      })
    }

  def select[T]: QueryBuilder[Select[T]] = {
    t: Select[T] =>
      Query("SELECT") :++
        t.terms.toSeq
          .map(termBuilder.apply)
          .reduceOption(_ :+ "," :++ _)
          .getOrElse(Query("1")) :++
        Query("FROM") :++
        t.joins
          .foldLeft(dataSourceWithAliasBuilder(t.dataSource))((from, join) => from :++ joinBuilder(join)) :++
        Query("WHERE") :++
        filterBuilder(t.filter) :++
        t.groupedBy
          .map(termSrcQueryBuilder.apply)
          .reduceOption(_ :+ "," :++ _)
          .map(clauses => Query("GROUP BY") :++ clauses) :++
        t.sort
          .map(sortBuilder.apply)
          .reduceOption(_ :+ "," :++ _)
          .map(sort => Query("ORDER BY") :++ sort) :++
        t.limit.map(limit =>
          Query("OFFSET") :++ limit.start.toString :++
            Query("LIMIT") :++ limit.stop.toString)
  }

  def insert[T]: QueryBuilder[Insert[T]] =
    (t: Insert[T]) =>
      Query("INSERT INTO") :++
        t.table.tableName :++
        t.columnValues
          .map(colValue => Query(colValue.column.name))
          .reduce(_ :+ "," :++ _)
          .wrap("(", ")") :++
        Query("VALUES") :++
        t.columnValues
          .map(columnValue => valueBuilder(columnValue.value))
          .reduce(_ :+ ", " :+ _)
          .wrap("(", ")")

  def update[T]: QueryBuilder[Update[T]] =
    (t: Update[T]) =>
      Query("UPDATE") :++
        dataSourceWithAliasBuilder(t.table) :++
        Query("SET") :++
        t.values
          .map(colValue => Query(colValue.column.name) :++ Query("=") :++ valueBuilder(colValue.value))
          .reduce(_ :+ "," :++ _) :++
        Query("WHERE") :++
        filterBuilder(t.filter)

  def delete: QueryBuilder[Delete] =
    (t: Delete) =>
      Query("DELETE FROM") :++
        dataSourceWithAliasBuilder(t.table) :++
        Query("WHERE") :++
        filterBuilder(t.filter)
}


object PostgresQueryBuilder {
  val build: QueryBuilder[Statement] = (statement: Statement) => {
    val builder = new PostgresQueryBuilder(new AliasLookup("ds"))

    statement match {
      case s: Select[_] => builder.select(s)
      case s: Insert[_] => builder.insert(s)
      case s: Update[_] => builder.update(s)
      case s: Delete => builder.delete(s)
    }
  }
}
