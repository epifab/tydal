import domain._

import scala.language.implicitConversions

case class Evaluated(src: String, params: Seq[Any]) {
  def `+`(e2: Evaluated) = Evaluated(s"$src${e2.src}", params ++ e2.params)
  def `+`(o: Option[Evaluated]): Evaluated = o.map(e => this + e).getOrElse(this)

  def `++`(e2: Evaluated) = Evaluated(s"$src ${e2.src}", params ++ e2.params)
  def `++`(o: Option[Evaluated]): Evaluated = o.map(e => this ++ e).getOrElse(this)
}

object Evaluated {
  def apply(src: String, params: Seq[Any] = Seq.empty): Evaluated = new Evaluated(src, params)
}

trait Evaluator[T] {
  def apply(t: T): Evaluated
}

trait QueryBuilder {
  def queryEval: Evaluator[SelectQuery]
}


object PostgresQueryBuilder extends QueryBuilder {
  implicit def fromString(s: String): Evaluated = Evaluated(s)

  val filterOpEval: Evaluator[Filter.Expression.Op] = {
    case Filter.Expression.Op.Equal => Evaluated("=")
    case Filter.Expression.Op.NotEqual => Evaluated("<>")
    case Filter.Expression.Op.Like => Evaluated("LIKE")
  }

  val filterClauseEval: Evaluator[Filter.Expression.Clause[_]] = {
    case f: Filter.Expression.Clause.Field[_] => Evaluated(f.field.src)
    case v: Filter.Expression.Clause.Value[_] => Evaluated("?", Seq(v.value))
  }

  val filterExpressionEval: Evaluator[Filter.Expression] = {
    case Filter.Expression(left, right, op) =>
      filterClauseEval(left) ++ filterOpEval(op) ++ filterClauseEval(right)
  }

  val filterEval: Evaluator[Filter] = {
    case e: Filter.Expression => filterExpressionEval(e)
    case Filter.And(f1, f2) => filterEval(f1) ++ "AND" ++ filterEval(f2)
    case Filter.Or(f1, f2) => filterEval(f1) ++ "OR" ++ filterEval(f2)
  }

  val fieldEval: Evaluator[Field[_]] =
    (t: Field[_]) => Evaluated(s"${t.src} AS ${t.alias}")

  val dataSourceEval: Evaluator[DataSource] =
    (t: DataSource) => Evaluated(s"${t.src} AS ${t.alias}")

  val joinEval: Evaluator[Join] =
    (t: Join) => {
      val clauses = t.clauses
        .map(filterEval.apply)
        .reduce((a, b) => a ++ "AND" ++ b)

      t.joinType match {
        case InnerJoin => Evaluated("INNER JOIN") ++ dataSourceEval(t.source) ++ "ON" ++ clauses
        case LeftJoin => Evaluated("LEFT JOIN") ++ dataSourceEval(t.source) ++ "ON" ++ clauses
      }
    }

  val queryEval: Evaluator[SelectQuery] =
    (t: SelectQuery) =>
      Evaluated("SELECT") ++
        t.fields
          .map(fieldEval.apply)
          .reduceOption(_ + "," ++ _)
          .getOrElse(Evaluated("1")) ++
      Evaluated("FROM") ++
        t.joins
          .foldLeft(dataSourceEval(t.dataSource))((from, join) => from ++ joinEval(join)) ++
      Evaluated("WHERE") ++
        t.filters
          .map(filterEval.apply)
          .reduceOption(_ ++ _)
          .getOrElse(Evaluated("1 = 1"))
}
