package domain

case class SelectQuery(
  dataSource: DataSource,
  fields: Seq[Field[_]] = Seq.empty,
  joins: Seq[Join] = Seq.empty,
  filter: Filter = Filter.Empty
) {
  def take(fields: Field[_]*): SelectQuery =
    copy(fields = this.fields ++ fields)

  def leftJoin(dataSource: DataSource, where: Filter = Filter.Empty): SelectQuery =
    copy(joins = joins :+ LeftJoin(dataSource, where))

  def innerJoin(dataSource: DataSource, where: Filter = Filter.Empty): SelectQuery =
    copy(joins = joins :+ InnerJoin(dataSource, where))

  def crossJoin(dataSource: DataSource): SelectQuery =
    copy(joins = joins :+ CrossJoin(dataSource))

  def where(filter: Filter): SelectQuery =
    copy(filter = this.filter and filter)
}

object SelectQuery {
  def from(dataSource: DataSource) = SelectQuery(dataSource)
}
