package io.epifab.yadl.domain

import scala.language.higherKinds

case class Query(sql: String, params: Seq[Value[_]]) {
  def :+(s: String) = Query(sql + s, params)
  def :+(q2: Query) = Query(sql + q2.sql, params ++ q2.params)
  def :+(oq: Option[Query]): Query = oq.map(e => this :+ e).getOrElse(this)

  def :++(s: String) = Query(sql + " " + s, params)
  def :++(e2: Query) = Query(sql + " " + e2.sql, params ++ e2.params)
  def :++(oq: Option[Query]): Query = oq.map(e => this :++ e).getOrElse(this)

  def wrap(before: String, after: String): Query = copy(before + sql + after)
}

object Query {
  def apply(src: String, params: Seq[Value[_]] = Seq.empty): Query = new Query(src, params)

  val empty: Query = Query("")
}
