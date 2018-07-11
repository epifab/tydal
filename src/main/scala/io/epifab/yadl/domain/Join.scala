package io.epifab.yadl.domain

sealed trait Join {
  def table: Table
}

final case class LeftJoin(override val table: Table, clauses: Filter) extends Join
final case class InnerJoin(override val table: Table, clauses: Filter) extends Join
final case class CrossJoin(override val table: Table) extends Join
