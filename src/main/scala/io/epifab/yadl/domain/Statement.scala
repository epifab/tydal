package io.epifab.yadl.domain

sealed trait Statement

sealed trait SideEffect

sealed trait SelectInterface {
  def columns: Seq[Column[_]]
  def aggregations: Seq[AggregateColumn[_, _]]
}

sealed trait TypedSelect[V, C] extends Statement with SelectInterface {
  def dataSource: DataSource
  def selectable: Selectable[V, C]
  def joins: Seq[Join]
  def filter: Filter
  def sort: Seq[Sort]
  def limit: Option[Limit]

  override def columns: Seq[Column[_]] = selectable.columns

  override def aggregations: Seq[AggregateColumn[_, _]] = selectable.aggregations

  def take[V2, C2](selectable: Selectable[V2, C2]): TypedSelect[V2, C2]

  def leftJoin[T <: DataSource](relation: Relation[T]): TypedSelect[V, C]

  def innerJoin[T <: DataSource](relation: Relation[T]): TypedSelect[V, C]

  def crossJoin(dataSource: DataSource): TypedSelect[V, C]

  def where(filter: Filter): TypedSelect[V, C]

  def sortBy(sort: Sort*): TypedSelect[V, C]

  def inRange(start: Int, stop: Int): TypedSelect[V, C]
}

object TypedSelect {
  case class TypedSelectImpl[V, C](
    dataSource: DataSource,
    selectable: Selectable[V, C],
    joins: Seq[Join] = Seq.empty,
    filter: Filter = Filter.Empty,
    sort: Seq[Sort] = Seq.empty,
    limit: Option[Limit] = None
  ) extends TypedSelect[V, C] {

    override def take[V2, C2](selectable: Selectable[V2, C2]): TypedSelect[V2, C2] =
      copy(selectable = selectable)

    def leftJoin[T <: DataSource](relation: Relation[T]): TypedSelect[V, C] =
      copy(joins = joins :+ LeftJoin(relation, relation.clause))

    def innerJoin[T <: DataSource](relation: Relation[T]): TypedSelect[V, C] =
      copy(joins = joins :+ InnerJoin(relation, relation.clause))

    def crossJoin(dataSource: DataSource): TypedSelect[V, C] =
      copy(joins = joins :+ CrossJoin(dataSource))

    def where(filter: Filter): TypedSelect[V, C] =
      copy(filter = this.filter and filter)

    def sortBy(sort: Sort*): TypedSelect[V, C] =
      copy(sort = this.sort ++ sort)

    def inRange(start: Int, stop: Int): TypedSelect[V, C] =
      copy(limit = Some(Limit(start, stop)))
  }

  def from(dataSource: DataSource): TypedSelect[Unit, Unit] = TypedSelectImpl(dataSource, SNil)
}

sealed trait Select extends Statement with SelectInterface {
  def dataSource: DataSource
  def columns: Seq[Column[_]]
  def aggregations: Seq[AggregateColumn[_, _]]
  def joins: Seq[Join]
  def filter: Filter
  def sort: Seq[Sort]
  def limit: Option[Limit]

  def take(column: Column[_], columns: Column[_]*): Select

  def take(columns: Seq[Column[_]]): Select

  def aggregateBy(aggregation: AggregateColumn[_, _], aggregations: AggregateColumn[_, _]*): Select

  def aggregateBy(aggregations: Seq[AggregateColumn[_, _]]): Select

  def leftJoin[T <: DataSource](relation: Relation[T]): Select

  def innerJoin[T <: DataSource](relation: Relation[T]): Select

  def crossJoin(dataSource: DataSource): Select

  def where(filter: Filter): Select

  def sortBy(sort: Sort*): Select

  def inRange(start: Int, stop: Int): Select
}

object Select {
  protected final case class SelectImpl(
    dataSource: DataSource,
    columns: Seq[Column[_]] = Seq.empty,
    aggregations: Seq[AggregateColumn[_, _]] = Seq.empty,
    joins: Seq[Join] = Seq.empty,
    filter: Filter = Filter.Empty,
    sort: Seq[Sort] = Seq.empty,
    limit: Option[Limit] = None
  ) extends Select {
    def take(columns: Seq[Column[_]]): Select =
      copy(columns = this.columns ++ columns)

    def take(column: Column[_], columns: Column[_]*): Select =
      copy(columns = this.columns ++ (column +: columns))

    def aggregateBy(aggregations: Seq[AggregateColumn[_, _]]): Select =
      copy(aggregations = this.aggregations ++ aggregations)

    def aggregateBy(aggregation: AggregateColumn[_, _], aggregations: AggregateColumn[_, _]*): Select =
      copy(aggregations = this.aggregations ++ (aggregation +: aggregations))

    def leftJoin[T <: DataSource](relation: Relation[T]): Select =
      copy(joins = joins :+ LeftJoin(relation, relation.clause))

    def innerJoin[T <: DataSource](relation: Relation[T]): Select =
      copy(joins = joins :+ InnerJoin(relation, relation.clause))

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
