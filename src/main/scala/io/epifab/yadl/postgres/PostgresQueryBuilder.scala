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


class PostgresQueryBuilder(aliasLookup: AliasLookup[DataSource]) {
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
        case _ => "?"
      }

      Query(toPlaceholder(value.adapter.dbType), Seq(value))
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
}


object PostgresQueryBuilder {
  val build: QueryBuilder[Statement] = (statement: Statement) => {
    val builder = new PostgresQueryBuilder(new AliasLookup[DataSource]("ds"))

    statement match {
      case s: Select => builder.select(s)
      case s: Insert => builder.insert(s)
      case s: Update => builder.update(s)
      case s: Delete => builder.delete(s)
    }
  }
}
