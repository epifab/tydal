package domain

case class SelectQuery(
  dataSource: DataSource,
  fields: Seq[Field] = Seq.empty,
  joins: Seq[Join] = Seq.empty,
  filters: Seq[Filter] = Seq.empty
)
