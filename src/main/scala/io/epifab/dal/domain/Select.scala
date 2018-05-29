package io.epifab.dal.domain

case class Select(
  dataSource: DataSource,
  fields: Seq[Field[_]] = Seq.empty,
  joins: Seq[Join] = Seq.empty,
  filter: Filter = Filter.Empty
) {
  def take(fields: Field[_]*): Select =
    copy(fields = this.fields ++ fields)

  def leftJoin(dataSource: DataSource, where: Filter = Filter.Empty): Select =
    copy(joins = joins :+ LeftJoin(dataSource, where))

  def innerJoin(dataSource: DataSource, where: Filter = Filter.Empty): Select =
    copy(joins = joins :+ InnerJoin(dataSource, where))

  def crossJoin(dataSource: DataSource): Select =
    copy(joins = joins :+ CrossJoin(dataSource))

  def where(filter: Filter): Select =
    copy(filter = this.filter and filter)
}

object Select {
  def from(dataSource: DataSource) = Select(dataSource)
}
