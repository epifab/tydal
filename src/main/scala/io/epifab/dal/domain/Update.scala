package io.epifab.dal.domain

case class Update(dataSource: DataSource, fieldValues: Seq[FieldValue[_]] = Seq.empty, filter: Filter = Filter.Empty) {
  def set(fieldValues: FieldValue[_]*): Update =
    copy(fieldValues = this.fieldValues ++ fieldValues)

  def where(filter: Filter): Update =
    copy(filter = this.filter and filter)
}
