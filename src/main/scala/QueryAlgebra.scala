import domain._

import scala.language.higherKinds


trait QueryAlgebra[T[_]] {
  def select: T[SelectQuery]
}

case class Query(query: String, params: Seq[Any]) {
  def `+`(e2: Query) = Query(s"$query${e2.query}", params ++ e2.params)
  def `+`(o: Option[Query]): Query = o.map(e => this + e).getOrElse(this)

  def `++`(e2: Query) = Query(s"$query ${e2.query}", params ++ e2.params)
  def `++`(o: Option[Query]): Query = o.map(e => this ++ e).getOrElse(this)
}

object Query {
  def apply(src: String, params: Seq[Any] = Seq.empty): Query = new Query(src, params)
  val empty: Query = Query("")
}

trait QueryBuilder[T] {
  def apply(t: T): Query
}

object PostgresQueryInterpreter extends QueryAlgebra[QueryBuilder] {
  implicit val fromString: String => Query = Query(_)

  val filterOpBuilder: QueryBuilder[Filter.Expression.Op] = {
    case Filter.Expression.Op.Equal => Query("=")
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
  }

  val fieldBuilder: QueryBuilder[Field[_]] =
    (field: Field[_]) => Query(s"${field.src} AS ${field.alias}")

  val dataSourceBuilder: QueryBuilder[DataSource] =
    (ds: DataSource) => Query(s"${ds.src} AS ${ds.alias}")

  val joinBuilder: QueryBuilder[Join] =
    (join: Join) => {
      val clauses = join.clauses
        .map(filterBuilder.apply)
        .reduce((a, b) => a ++ "AND" ++ b)

      join.joinType match {
        case InnerJoin => Query("INNER JOIN") ++ dataSourceBuilder(join.source) ++ "ON" ++ clauses
        case LeftJoin => Query("LEFT JOIN") ++ dataSourceBuilder(join.source) ++ "ON" ++ clauses
      }
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
        t.filters
          .map(filterBuilder.apply)
          .reduceOption(_ ++ _)
          .getOrElse(Query("1 = 1"))
}
