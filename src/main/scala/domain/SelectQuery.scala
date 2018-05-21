package domain

case class SelectQuery(
  dataSource: DataSource,
  fields: Seq[Field[_]] = Seq.empty,
  joins: Seq[Join] = Seq.empty,
  filter: Filter = Filter.Empty
) {
  def take(fields: Seq[Field[_]]): SelectQuery =
    copy(fields = this.fields ++ fields)

  def leftJoin(dataSource: DataSource, where: Filter = Filter.Empty): SelectQuery =
    copy(joins = joins :+ Join(dataSource, LeftJoin, where))

  def innerJoin(dataSource: DataSource, where: Filter = Filter.Empty): SelectQuery =
    copy(joins = joins :+ Join(dataSource, InnerJoin, where))

  def where(filter: Filter): SelectQuery =
    copy(filter = this.filter and filter)
}
