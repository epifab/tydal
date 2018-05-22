package domain

sealed trait Join {
  def source: DataSource
}

final case class LeftJoin(override val source: DataSource, clauses: Filter) extends Join
final case class InnerJoin(override val source: DataSource, clauses: Filter) extends Join
final case class CrossJoin(override val source: DataSource) extends Join
