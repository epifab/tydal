package io.epifab.dal.domain

case class Insert(dataSource: DataSource, fieldValues: Seq[FieldValue[_]] = Seq.empty) {
  def set(fieldValues: FieldValue[_]*): Insert =
    copy(fieldValues = this.fieldValues ++ fieldValues)
}

object Insert {
  def into(dataSource: DataSource) = Insert(dataSource)
}
