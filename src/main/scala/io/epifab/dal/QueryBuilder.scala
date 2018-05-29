package io.epifab.dal

import scala.language.higherKinds

case class Query(query: String, params: Seq[Any]) {
  def +(e2: Query) = Query(query + e2.query, params ++ e2.params)
  def +(o: Option[Query]): Query = o.map(e => this + e).getOrElse(this)

  def ++(e2: Query) = Query(query + " " + e2.query, params ++ e2.params)
  def ++(o: Option[Query]): Query = o.map(e => this ++ e).getOrElse(this)

  def wrap(before: String, after: String): Query = copy(before + query + after)
}

object Query {
  def apply(src: String, params: Seq[Any] = Seq.empty): Query = new Query(src, params)
  val empty: Query = Query("")
}

trait QueryBuilder[T] {
  def apply(t: T): Query
}
