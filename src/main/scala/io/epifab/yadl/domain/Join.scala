package io.epifab.yadl.domain

sealed trait Join {
  def dataSource: DataSource
}

final case class LeftJoin(override val dataSource: DataSource, clauses: Filter) extends Join
final case class InnerJoin(override val dataSource: DataSource, clauses: Filter) extends Join
final case class CrossJoin(override val dataSource: DataSource) extends Join
