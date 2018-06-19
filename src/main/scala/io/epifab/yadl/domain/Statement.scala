package io.epifab.yadl.domain

sealed trait Statement

sealed trait SideEffect

sealed trait Select extends Statement {
  def dataSource: DataSource
  def fields: Seq[Field[_, _]]
  def joins: Seq[Join]
  def filter: Filter
  def sort: Seq[Sort]
  def limit: Option[Limit]

  def take(field: Field[_, _], fields: Field[_, _]*): Select

  def take(fields: Seq[Field[_, _]]): Select

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
    fields: Seq[Field[_, _]] = Seq.empty,
    joins: Seq[Join] = Seq.empty,
    filter: Filter = Filter.Empty,
    sort: Seq[Sort] = Seq.empty,
    limit: Option[Limit] = None
  ) extends Select {
    def take(fields: Seq[Field[_, _]]): Select =
      copy(fields = this.fields ++ fields)

    def take(field: Field[_, _], fields: Field[_, _]*): Select =
      copy(fields = this.fields ++ (field +: fields))

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
  def fieldValues: Seq[FieldValue[_, _]]

  def set(fieldValues: FieldValue[_, _]*): Insert
}

object Insert {
  protected final case class InsertImpl(dataSource: DataSource, fieldValues: Seq[FieldValue[_, _]] = Seq.empty) extends Insert {
    def set(fieldValues: FieldValue[_, _]*): Insert =
      copy(fieldValues = this.fieldValues ++ fieldValues)
  }

  def into(dataSource: DataSource) = InsertImpl(dataSource)
}

sealed trait Update extends Statement with SideEffect {
  def dataSource: DataSource
  def fieldValues: Seq[FieldValue[_, _]]
  def filter: Filter

  def set(fieldValues: FieldValue[_, _]*): Update
  def where(filter: Filter): Update
}

object Update {
  protected final case class UpdateImpl(dataSource: DataSource, fieldValues: Seq[FieldValue[_, _]] = Seq.empty, filter: Filter = Filter.Empty) extends Update {
    def set(fieldValues: FieldValue[_, _]*): Update =
      copy(fieldValues = this.fieldValues ++ fieldValues)

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
