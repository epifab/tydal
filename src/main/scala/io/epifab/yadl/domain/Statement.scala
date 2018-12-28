package io.epifab.yadl.domain

import shapeless.HNil

sealed trait Statement

sealed trait SideEffect

sealed trait SelectInterface {
  def columns: Seq[Column[_]]
  def aggregations: Seq[AggregateColumn[_, _]]
}

sealed trait TypedSelect[V] extends Statement with SelectInterface {
  def dataSource: DataSource
  def selectable: Selectable[V]
  def joins: Seq[Join]
  def filter: Filter
  def sort: Seq[Sort]
  def limit: Option[Limit]

  override def columns: Seq[Column[_]] = selectable.columns

  override def aggregations: Seq[AggregateColumn[_, _]] = selectable.aggregations

  def take[V2](selectable: Selectable[V2]): TypedSelect[V2]

  def leftJoin[T <: DataSource](relation: Relation[T]): TypedSelect[V]

  def innerJoin[T <: DataSource](relation: Relation[T]): TypedSelect[V]

  def crossJoin(dataSource: DataSource): TypedSelect[V]

  def where(filter: Filter): TypedSelect[V]

  def sortBy(sort: Sort*): TypedSelect[V]

  def inRange(start: Int, stop: Int): TypedSelect[V]
}

object TypedSelect {
  case class TypedSelectImpl[V](
    dataSource: DataSource,
    selectable: Selectable[V],
    joins: Seq[Join] = Seq.empty,
    filter: Filter = Filter.Empty,
    sort: Seq[Sort] = Seq.empty,
    limit: Option[Limit] = None
  ) extends TypedSelect[V] {

    override def take[V2](selectable: Selectable[V2]): TypedSelect[V2] =
      copy(selectable = selectable)

    def leftJoin[T <: DataSource](relation: Relation[T]): TypedSelect[V] =
      copy(joins = joins :+ LeftJoin(relation, relation.clause))

    def innerJoin[T <: DataSource](relation: Relation[T]): TypedSelect[V] =
      copy(joins = joins :+ InnerJoin(relation, relation.clause))

    def crossJoin(dataSource: DataSource): TypedSelect[V] =
      copy(joins = joins :+ CrossJoin(dataSource))

    def where(filter: Filter): TypedSelect[V] =
      copy(filter = this.filter and filter)

    def sortBy(sort: Sort*): TypedSelect[V] =
      copy(sort = this.sort ++ sort)

    def inRange(start: Int, stop: Int): TypedSelect[V] =
      copy(limit = Some(Limit(start, stop)))
  }

  def from(dataSource: DataSource): TypedSelect[HNil] = TypedSelectImpl(dataSource, SNil)
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

sealed trait Insert[T] extends Statement with SideEffect {
  def table: Table[T]
  def columnValues: Seq[ColumnValue[_]]

  def set(columnValues: ColumnValue[_]*): Insert[T]
}

object Insert {
  protected final case class InsertImpl[T](table: Table[T], columnValues: Seq[ColumnValue[_]] = Seq.empty) extends Insert[T] {
    def set(columnValues: ColumnValue[_]*): Insert[T] =
      copy(columnValues = this.columnValues ++ columnValues)
  }

  def into[T](table: Table[T]) = InsertImpl(table)
}

sealed trait Update[T] extends Statement with SideEffect {
  def table: Table[T]
  def values: Seq[ColumnValue[_]]
  def filter: Filter

  def set(columnValues: ColumnValue[_]*): Update[T]
  def where(filter: Filter): Update[T]
}

object Update {
  protected final case class UpdateImpl[T](table: Table[T], values: Seq[ColumnValue[_]] = Seq.empty, filter: Filter = Filter.Empty) extends Update[T] {
    def set(columnValues: ColumnValue[_]*): Update[T] =
      copy(values = this.values ++ columnValues)

    def where(filter: Filter): Update[T] =
      copy(filter = this.filter and filter)
  }

  def apply[T](table: Table[T]) = UpdateImpl(table)
}

sealed trait Delete extends Statement with SideEffect {
  def table: Table[_]
  def filter: Filter

  def where(filter: Filter): Delete
}

object Delete {
  protected final case class DeleteImpl(table: Table[_], filter: Filter = Filter.Empty) extends Delete {
    def where(filter: Filter): Delete =
      copy(filter = this.filter and filter)
  }

  def apply(table: Table[_]) = DeleteImpl(table)
}
