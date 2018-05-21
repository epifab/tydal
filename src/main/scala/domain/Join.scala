package domain

sealed trait JoinType
case object InnerJoin extends JoinType
case object LeftJoin extends JoinType

case class Join(source: DataSource, joinType: JoinType, clauses: Filter = Filter.Empty)
