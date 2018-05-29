package io.epifab.dal.domain

trait Update {
  def dataSource: DataSource
  def fieldValues: Seq[FieldValue[_]]
  def filter: Filter

  def set(fieldValues: FieldValue[_]*): Update
  def where(filter: Filter): Update
}

object Update {
  protected case class UpdateImpl(dataSource: DataSource, fieldValues: Seq[FieldValue[_]] = Seq.empty, filter: Filter = Filter.Empty) extends Update {
    def set(fieldValues: FieldValue[_]*): Update =
      copy(fieldValues = this.fieldValues ++ fieldValues)

    def where(filter: Filter): Update =
      copy(filter = this.filter and filter)
  }

  def apply(dataSource: DataSource) = UpdateImpl(dataSource)
}
