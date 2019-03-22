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

    case Conversion1(term, conversionFunction) =>
      termSrcQueryBuilder(term).wrap(s"${conversionFunction.name}(", ")")

    case Conversion2(term1, term2, conversionFunction) =>
      (termSrcQueryBuilder(term1) :+ "," :++ termSrcQueryBuilder(term2))
        .wrap(s"${conversionFunction.name}(", ")")

    case SubQueryTerm(term, subQuery) =>
      Query(aliasLookup(subQuery)) :+ "." :+ termAliasQueryBuilder(term)

    case Distinct(term) =>
      Query("DISTINCT") :++ termSrcQueryBuilder(term)

    case value: Value[_] =>
      valueBuilder(value)
  }

  def termAliasQueryBuilder: QueryBuilder[Term[_]] = {
    case Column(name, table) =>
      Query(aliasLookup(table) + "__" + name)

    case Aggregation(term, aggregateFunction) =>
      Query(aggregateFunction.name) :+ "_" :+ termAliasQueryBuilder(term)

    case Conversion1(term, conversionFunction) =>
      Query(conversionFunction.name) :+ "_" :+ termAliasQueryBuilder(term)

    case Conversion2(term1, term2, conversionFunction) =>
      Query(conversionFunction.name) :+ "_" :+ termAliasQueryBuilder(term1) :+ "_" :+ termAliasQueryBuilder(term2)

    case SubQueryTerm(term, subQuery) =>
      Query(aliasLookup(subQuery)) :+ "__" :+ termAliasQueryBuilder(term)

    case Distinct(term) =>
      termAliasQueryBuilder(term)

    case value: Value[_] =>
      Query(aliasLookup(value))
  }

  def filterExpressionBuilder: QueryBuilder[BinaryExpr] = {
    case AlwaysTrue =>
      Query("1 = 1")

    case And(left, right) =>
      filterExpressionBuilder(left) :++ "AND" :++ filterExpressionBuilder(right)

    case Or(left, op) =>
      (filterExpressionBuilder(left) :++ "OR" :++ filterExpressionBuilder(op)).wrap

    case Equals(term1, term2) =>
      termSrcQueryBuilder(term1) :++ "=" :++ termSrcQueryBuilder(term2)

    case NotEquals(term1, term2) =>
      termSrcQueryBuilder(term1) :++ "!=" :++ termSrcQueryBuilder(term2)

    case GreaterThan(term1, term2) =>
      termSrcQueryBuilder(term1) :++ ">" :++ termSrcQueryBuilder(term2)

    case LessThan(term1, term2) =>
      termSrcQueryBuilder(term1) :++ "<" :++ termSrcQueryBuilder(term2)

    case GreaterThanOrEqual(term1, term2) =>
      termSrcQueryBuilder(term1) :++ ">=" :++ termSrcQueryBuilder(term2)

    case LessThanOrEqual(term1, term2) =>
      termSrcQueryBuilder(term1) :++ "<=" :++ termSrcQueryBuilder(term2)

    case Like(term1, term2) =>
      termSrcQueryBuilder(term1) :++ "LIKE" :++ termSrcQueryBuilder(term2)

    case IsDefined(term) =>
      termSrcQueryBuilder(term) :++ "IS NOT NULL"

    case IsNotDefined(term) =>
      termSrcQueryBuilder(term) :++ "IS NULL"

    case IsSuperset(terms1, terms2) =>
      termSrcQueryBuilder(terms1) :++ "@>" :++ termSrcQueryBuilder(terms2)

    case IsSubset(terms1, terms2) =>
      termSrcQueryBuilder(terms1) :++ "<@" :+ termSrcQueryBuilder(terms2)

    case Overlaps(terms1, terms2) =>
      termSrcQueryBuilder(terms1) :++ "&&" :++ termSrcQueryBuilder(terms2)

    case IsIncluded(term, terms) =>
      termSrcQueryBuilder(term) :++ "= ANY" :+ termSrcQueryBuilder(terms).wrap
  }

  def termBuilder: QueryBuilder[Term[_]] =
    (term: Term[_]) => termSrcQueryBuilder(term) :++ "AS" :++ termAliasQueryBuilder(term)

  def dataSourceWithAliasBuilder: QueryBuilder[DataSource] = {
    case dataSource: Table[_] =>
      Query(dataSource.tableName + " AS " + aliasLookup(dataSource))
    case dataSource: TableProjection[_, _] =>
      Query(dataSource.table.tableName + " AS " + aliasLookup(dataSource.table))
    case dataSource: SubQuery[_, _] =>
      select(dataSource.select).wrap :++ "AS" :++ aliasLookup(dataSource)
  }

  def joinBuilder: QueryBuilder[Join] = {
    case InnerJoin(source, clauses) =>
      Query("INNER JOIN") :++ dataSourceWithAliasBuilder(source) :++ "ON" :++ filterExpressionBuilder(clauses)
    case LeftJoin(source, clauses) =>
      Query("LEFT JOIN") :++ dataSourceWithAliasBuilder(source) :++ "ON" :++ filterExpressionBuilder(clauses)
    case CrossJoin(source) =>
      Query("CROSS JOIN") :++ dataSourceWithAliasBuilder(source)
  }

  def sortBuilder: QueryBuilder[Sort] =
    (s: Sort) => {
      termSrcQueryBuilder(s.term) :++ (s match {
        case _: Asc => Query("ASC")
        case _: Desc => Query("DESC")
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
        filterExpressionBuilder(t.filter) :++
        t.groupedBy
          .map(termSrcQueryBuilder.apply)
          .reduceOption(_ :+ "," :++ _)
          .map(clauses => Query("GROUP BY") :++ clauses) :++
        t.sort
          .map(sortBuilder.apply)
          .reduceOption(_ :+ "," :++ _)
          .map(sort => Query("ORDER BY") :++ sort) :++
        t.limit.map(limit =>
          Query("OFFSET") :++ limit.offset.toString :++
            Query("LIMIT") :++ limit.limit.toString)
  }

  def insert[T]: QueryBuilder[Insert[T]] =
    (t: Insert[T]) =>
      Query("INSERT INTO") :++
        t.table.tableName :++
        t.columnValues
          .map(colValue => Query(colValue.column.name))
          .reduce(_ :+ "," :++ _)
          .wrap :++
        Query("VALUES") :++
        t.columnValues
          .map(columnValue => valueBuilder(columnValue.value))
          .reduce(_ :+ ", " :+ _)
          .wrap

  def update[T]: QueryBuilder[Update[T]] =
    (t: Update[T]) =>
      Query("UPDATE") :++
        dataSourceWithAliasBuilder(t.table) :++
        Query("SET") :++
        t.values
          .map(colValue => Query(colValue.column.name) :++ Query("=") :++ valueBuilder(colValue.value))
          .reduce(_ :+ "," :++ _) :++
        Query("WHERE") :++
        filterExpressionBuilder(t.filter)

  def delete: QueryBuilder[Delete] =
    (t: Delete) =>
      Query("DELETE FROM") :++
        dataSourceWithAliasBuilder(t.table) :++
        Query("WHERE") :++
        filterExpressionBuilder(t.filter)
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
