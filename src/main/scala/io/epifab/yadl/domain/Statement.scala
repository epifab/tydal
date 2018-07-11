package io.epifab.yadl.domain

sealed trait Statement

sealed trait SideEffect

sealed trait Select extends Statement {
  def table: Table
  def columns: Seq[TableColumn[_]]
  def aggregations: Seq[Column[_]]
  def joins: Seq[Join]
  def filter: Filter
  def sort: Seq[Sort]
  def limit: Option[Limit]

  def take(column: TableColumn[_], columns: TableColumn[_]*): Select

  def take(columns: Seq[TableColumn[_]]): Select

  def aggregateBy(aggregations: AggregateColumn[_, _]*): Select

  def leftJoin(relation: Table with Relation): Select

  def leftJoin(table: Table, where: Filter = Filter.Empty): Select

  def innerJoin(relation: Table with Relation): Select

  def innerJoin(table: Table, where: Filter = Filter.Empty): Select

  def crossJoin(table: Table): Select

  def where(filter: Filter): Select

  def sortBy(sort: Sort*): Select

  def inRange(start: Int, stop: Int): Select
}

object Select {
  protected final case class SelectImpl(
                                         table: Table,
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

    def leftJoin(relation: Table with Relation): Select =
      leftJoin(relation, relation.relationClause)

    def leftJoin(table: Table, where: Filter = Filter.Empty): Select =
      copy(joins = joins :+ LeftJoin(table, where))

    def innerJoin(relation: Table with Relation): Select =
      innerJoin(relation, relation.relationClause)

    def innerJoin(table: Table, where: Filter = Filter.Empty): Select =
      copy(joins = joins :+ InnerJoin(table, where))

    def crossJoin(table: Table): Select =
      copy(joins = joins :+ CrossJoin(table))

    def where(filter: Filter): Select =
      copy(filter = this.filter and filter)

    def sortBy(sort: Sort*): Select =
      copy(sort = this.sort ++ sort)

    def inRange(start: Int, stop: Int): Select =
      copy(limit = Some(Limit(start, stop)))
  }

  def from(table: Table) = SelectImpl(table)
}

sealed trait Insert extends Statement with SideEffect {
  def table: Table
  def columnValues: Seq[ColumnValue[_]]

  def set(columnValues: ColumnValue[_]*): Insert
}

object Insert {
  protected final case class InsertImpl(table: Table, columnValues: Seq[ColumnValue[_]] = Seq.empty) extends Insert {
    def set(columnValues: ColumnValue[_]*): Insert =
      copy(columnValues = this.columnValues ++ columnValues)
  }

  def into(table: Table) = InsertImpl(table)
}

sealed trait Update extends Statement with SideEffect {
  def table: Table
  def values: Seq[ColumnValue[_]]
  def filter: Filter

  def set(columnValues: ColumnValue[_]*): Update
  def where(filter: Filter): Update
}

object Update {
  protected final case class UpdateImpl(table: Table, values: Seq[ColumnValue[_]] = Seq.empty, filter: Filter = Filter.Empty) extends Update {
    def set(columnValues: ColumnValue[_]*): Update =
      copy(values = this.values ++ columnValues)

    def where(filter: Filter): Update =
      copy(filter = this.filter and filter)
  }

  def apply(table: Table) = UpdateImpl(table)
}

sealed trait Delete extends Statement with SideEffect {
  def table: Table
  def filter: Filter

  def where(filter: Filter): Delete
}

object Delete {
  protected final case class DeleteImpl(table: Table, filter: Filter = Filter.Empty) extends Delete {
    def where(filter: Filter): Delete =
      copy(filter = this.filter and filter)
  }

  def apply(table: Table) = DeleteImpl(table)
}
