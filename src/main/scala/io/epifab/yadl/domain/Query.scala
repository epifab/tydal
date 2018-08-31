package io.epifab.yadl.domain

import scala.language.higherKinds

case class Query(query: String, params: Seq[Value[_]]) {
  def :+(s: String) = Query(query + s, params)
  def :+(q2: Query) = Query(query + q2.query, params ++ q2.params)
  def :+(oq: Option[Query]): Query = oq.map(e => this :+ e).getOrElse(this)

  def :++(s: String) = Query(query + " " + s, params)
  def :++(e2: Query) = Query(query + " " + e2.query, params ++ e2.params)
  def :++(oq: Option[Query]): Query = oq.map(e => this :++ e).getOrElse(this)

  def wrap(before: String, after: String): Query = copy(before + query + after)
}

object Query {
  def apply(src: String, params: Seq[Value[_]] = Seq.empty): Query = new Query(src, params)

  val empty: Query = Query("")
}
