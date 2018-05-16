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
}

trait QueryBuilder[T] {
  def apply(t: T): Query
}

object PostgresQueryInterpreter extends QueryAlgebra[QueryBuilder] {
  implicit val fromString: String => Query = Query(_)

  val filterOpEval: QueryBuilder[Filter.Expression.Op] = {
    case Filter.Expression.Op.Equal => Query("=")
    case Filter.Expression.Op.NotEqual => Query("<>")
    case Filter.Expression.Op.Like => Query("LIKE")
    case Filter.Expression.Op.In => Query("IN")
  }

  val filterClauseEval: QueryBuilder[Filter.Expression.Clause[_]] = {
    case f: Filter.Expression.Clause.Field[_] => Query(f.field.src)
    case v: Filter.Expression.Clause.Value[_] => Query("?", Seq(v.value))
  }

  val filterExpressionEval: QueryBuilder[Filter.Expression] = {
    case Filter.Expression(left, right, op) =>
      filterClauseEval(left) ++ filterOpEval(op) ++ filterClauseEval(right)
  }

  val filterEval: QueryBuilder[Filter] = {
    case e: Filter.Expression => filterExpressionEval(e)
    case Filter.And(f1, f2) => filterEval(f1) ++ "AND" ++ filterEval(f2)
    case Filter.Or(f1, f2) => filterEval(f1) ++ "OR" ++ filterEval(f2)
  }

  val fieldEval: QueryBuilder[Field[_]] =
    (t: Field[_]) => Query(s"${t.src} AS ${t.alias}")

  val dataSourceEval: QueryBuilder[DataSource] =
    (t: DataSource) => Query(s"${t.src} AS ${t.alias}")

  val joinEval: QueryBuilder[Join] =
    (t: Join) => {
      val clauses = t.clauses
        .map(filterEval.apply)
        .reduce((a, b) => a ++ "AND" ++ b)

      t.joinType match {
        case InnerJoin => Query("INNER JOIN") ++ dataSourceEval(t.source) ++ "ON" ++ clauses
        case LeftJoin => Query("LEFT JOIN") ++ dataSourceEval(t.source) ++ "ON" ++ clauses
      }
    }

  val select: QueryBuilder[SelectQuery] =
    (t: SelectQuery) =>
      Query("SELECT") ++
        t.fields
          .map(fieldEval.apply)
          .reduceOption(_ + "," ++ _)
          .getOrElse(Query("1")) ++
      Query("FROM") ++
        t.joins
          .foldLeft(dataSourceEval(t.dataSource))((from, join) => from ++ joinEval(join)) ++
      Query("WHERE") ++
        t.filters
          .map(filterEval.apply)
          .reduceOption(_ ++ _)
          .getOrElse(Query("1 = 1"))
}
