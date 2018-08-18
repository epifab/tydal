package io.epifab.yadl.domain

import scala.language.higherKinds

case class Query(query: String, params: Seq[Value[_]]) {
  def +(e2: Query) = Query(query + e2.query, params ++ e2.params)
  def +(o: Option[Query]): Query = o.map(e => this + e).getOrElse(this)

  def ++(e2: Query) = Query(query + " " + e2.query, params ++ e2.params)
  def ++(o: Option[Query]): Query = o.map(e => this ++ e).getOrElse(this)

  def wrap(before: String, after: String): Query = copy(before + query + after)
}

object Query {
  def apply(src: String, params: Seq[Value[_]] = Seq.empty): Query = new Query(src, params)
  val empty: Query = Query("")
}
