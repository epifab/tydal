package io.epifab.dal.domain

trait Select {
  def dataSource: DataSource
  def fields: Seq[Field[_]]
  def joins: Seq[Join]
  def filter: Filter

  def take(fields: Field[_]*): Select

  def leftJoin(relation: DataSource with Relation): Select

  def leftJoin(dataSource: DataSource, where: Filter = Filter.Empty): Select

  def innerJoin(relation: DataSource with Relation): Select

  def innerJoin(dataSource: DataSource, where: Filter = Filter.Empty): Select

  def crossJoin(dataSource: DataSource): Select

  def where(filter: Filter): Select
}

object Select {
  protected case class SelectImpl(
    dataSource: DataSource,
    fields: Seq[Field[_]] = Seq.empty,
    joins: Seq[Join] = Seq.empty,
    filter: Filter = Filter.Empty
  ) extends Select {

    def take(fields: Field[_]*): Select =
      copy(fields = this.fields ++ fields)

    def leftJoin(relation: DataSource with Relation): Select =
      leftJoin(relation, relation.relationClause)

    def leftJoin(dataSource: DataSource, where: Filter = Filter.Empty): Select =
      copy(joins = joins :+ LeftJoin(dataSource, where))

    def innerJoin(relation: DataSource with Relation): Select =
      innerJoin(relation, relation.relationClause)

    def innerJoin(dataSource: DataSource, where: Filter = Filter.Empty): Select =
      copy(joins = joins :+ InnerJoin(dataSource, where))

    def crossJoin(dataSource: DataSource): Select =
      copy(joins = joins :+ CrossJoin(dataSource))

    def where(filter: Filter): Select =
      copy(filter = this.filter and filter)
  }

  def from(dataSource: DataSource) = SelectImpl(dataSource)
}
