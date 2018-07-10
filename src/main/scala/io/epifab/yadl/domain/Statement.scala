package io.epifab.yadl.domain

sealed trait Statement

sealed trait SideEffect

sealed trait Select extends Statement {
  def dataSource: DataSource
  def columns: Seq[TableColumn[_]]
  def aggregations: Seq[Column[_]]
  def joins: Seq[Join]
  def filter: Filter
  def sort: Seq[Sort]
  def limit: Option[Limit]

  def take(column: TableColumn[_], columns: TableColumn[_]*): Select

  def take(columns: Seq[TableColumn[_]]): Select

  def aggregateBy(aggregations: AggregateColumn[_, _]*): Select

  def leftJoin(relation: DataSource with Relation): Select

  def leftJoin(dataSource: DataSource, where: Filter = Filter.Empty): Select

  def innerJoin(relation: DataSource with Relation): Select

  def innerJoin(dataSource: DataSource, where: Filter = Filter.Empty): Select

  def crossJoin(dataSource: DataSource): Select

  def where(filter: Filter): Select

  def sortBy(sort: Sort*): Select

  def inRange(start: Int, stop: Int): Select
}

object Select {
  protected final case class SelectImpl(
                                         dataSource: DataSource,
                                         columns: Seq[TableColumn[_]] = Seq.empty,
                                         aggregations: Seq[Column[_]] = Seq.empty,
                                         joins: Seq[Join] = Seq.empty,
                                         filter: Filter = Filter.Empty,
                                         sort: Seq[Sort] = Seq.empty,
                                         limit: Option[Limit] = None
  ) extends Select {
    def take(columns: Seq[TableColumn[_]]): Select =
      copy(columns = this.columns ++ columns)

    def take(column: TableColumn[_], columns: TableColumn[_]*): Select =
      copy(columns = this.columns ++ (column +: columns))

    def aggregateBy(aggregations: AggregateColumn[_, _]*): Select =
      copy(aggregations = this.aggregations ++ aggregations)

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

    def sortBy(sort: Sort*): Select =
      copy(sort = this.sort ++ sort)

    def inRange(start: Int, stop: Int): Select =
      copy(limit = Some(Limit(start, stop)))
  }

  def from(dataSource: DataSource) = SelectImpl(dataSource)
}

sealed trait Insert extends Statement with SideEffect {
  def dataSource: DataSource
  def columnValues: Seq[ColumnValue[_]]

  def set(columnValues: ColumnValue[_]*): Insert
}

object Insert {
  protected final case class InsertImpl(dataSource: DataSource, columnValues: Seq[ColumnValue[_]] = Seq.empty) extends Insert {
    def set(columnValues: ColumnValue[_]*): Insert =
      copy(columnValues = this.columnValues ++ columnValues)
  }

  def into(dataSource: DataSource) = InsertImpl(dataSource)
}

sealed trait Update extends Statement with SideEffect {
  def dataSource: DataSource
  def values: Seq[ColumnValue[_]]
  def filter: Filter

  def set(columnValues: ColumnValue[_]*): Update
  def where(filter: Filter): Update
}

object Update {
  protected final case class UpdateImpl(dataSource: DataSource, values: Seq[ColumnValue[_]] = Seq.empty, filter: Filter = Filter.Empty) extends Update {
    def set(columnValues: ColumnValue[_]*): Update =
      copy(values = this.values ++ columnValues)

    def where(filter: Filter): Update =
      copy(filter = this.filter and filter)
  }

  def apply(dataSource: DataSource) = UpdateImpl(dataSource)
}

sealed trait Delete extends Statement with SideEffect {
  def dataSource: DataSource
  def filter: Filter

  def where(filter: Filter): Delete
}

object Delete {
  protected final case class DeleteImpl(dataSource: DataSource, filter: Filter = Filter.Empty) extends Delete {
    def where(filter: Filter): Delete =
      copy(filter = this.filter and filter)
  }

  def apply(dataSource: DataSource) = DeleteImpl(dataSource)
}
