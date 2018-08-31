package io.epifab.yadl.postgres

import java.time.{LocalDate, LocalDateTime}

import io.epifab.yadl.domain._


class AliasLookup[T](prefix: String) {
  private var count: Int = 0

  private val aliases: scala.collection.mutable.Map[T, String] =
    new scala.collection.mutable.HashMap

  private def nextAlias: String = synchronized {
    count += 1
    prefix + count.toString
  }

  def apply(t: T): String =
    aliases.getOrElseUpdate(t, nextAlias)
}

object PostgresQueryBuilder {
  def build: QueryBuilder[Statement] = new QueryBuilder[Statement] {
    val aliasLookup = new AliasLookup[DataSource]("ds")

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
        def toPlaceholder[T](t: T): String = t match {
          case Some(x) => toPlaceholder(x)
          case Json(x) => "cast(? as json)"
          case _: LocalDate => "cast(? as date)"
          case _: LocalDateTime => "cast(? as timestamp without time zone)"
          case _ => "?"
        }
        Query(toPlaceholder(value.value), Seq(value))
      }

    def columnSrcQueryBuilder: QueryBuilder[Column[_]] = {
      case TableColumn(name, table) =>
        Query(aliasLookup(table) + "." + name)

      case AggregateColumn(column, aggregateFunction) =>
        Query(aggregateFunction.name) :+ "(" :+ columnSrcQueryBuilder(column) :+ ")"

      case SubQueryColumn(column, subQuery) =>
        Query(aliasLookup(subQuery)) :+ "." :+ columnAliasQueryBuilder(column)
    }

    def columnAliasQueryBuilder: QueryBuilder[Column[_]] = {
      case TableColumn(name, table) =>
        Query(aliasLookup(table) + "__" + name)

      case AggregateColumn(column, aggregateFunction) =>
        Query(aggregateFunction.name) :+ "_" :+ columnAliasQueryBuilder(column)

      case SubQueryColumn(column, subQuery) =>
        Query(aliasLookup(subQuery)) :+ "__" :+ columnAliasQueryBuilder(column)
    }

    def filterClauseBuilder: QueryBuilder[Filter.Expression.Clause[_]] = {
      case Filter.Expression.Clause.Column(column) =>
        columnSrcQueryBuilder(column)

      case Filter.Expression.Clause.Literal(value) =>
        valueBuilder.apply(value)

      case Filter.Expression.Clause.AnyLiteral(values) =>
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

    def columnBuilder: QueryBuilder[Column[_]] =
      (column: Column[_]) => columnSrcQueryBuilder(column) :++ "AS" :++ columnAliasQueryBuilder(column)

    def dataSourceWithAliasBuilder: QueryBuilder[DataSource] = {
      case dataSource: Table =>
        Query(dataSource.tableName + " AS " + aliasLookup(dataSource))
      case dataSource: SubQuery =>
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
        columnSrcQueryBuilder(s.column) :++ (s match {
          case _: AscSort => Query("ASC")
          case _: DescSort => Query("DESC")
        })
      }

    def select: QueryBuilder[Select] =
      (t: Select) =>
        Query("SELECT") :++
          (t.columns ++ t.aggregations)
            .map(columnBuilder.apply)
            .reduceOption(_ :+ "," :++ _)
            .getOrElse(Query("1")) :++
          Query("FROM") :++
          t.joins
            .foldLeft(dataSourceWithAliasBuilder(t.dataSource))((from, join) => from :++ joinBuilder(join)) :++
          Query("WHERE") :++
          filterBuilder(t.filter) :++
          t.aggregations
            .headOption
            .flatMap(_ =>
              t.columns.map(columnSrcQueryBuilder.apply)
                .reduceOption(_ :+ "," :++ _)
                .map(columns => Query("GROUP BY") :++ columns)
            ) :++
          t.sort
            .map(sortBuilder.apply)
            .reduceOption(_ :+ "," :++ _)
            .map(sort => Query("ORDER BY") :++ sort) :++
          t.limit.map(limit =>
            Query("OFFSET") :++ limit.start.toString :++
              Query("LIMIT") :++ limit.stop.toString)

    def insert: QueryBuilder[Insert] =
      (t: Insert) =>
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

    def update: QueryBuilder[Update] =
      (t: Update) =>
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

    override def apply(statement: Statement): Query = statement match {
      case s: Select => select(s)
      case s: Insert => insert(s)
      case s: Update => update(s)
      case s: Delete => delete(s)
    }
  }
}
