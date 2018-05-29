package io.epifab.dal.domain

case class Delete(dataSource: DataSource, filter: Filter = Filter.Empty) {
  def where(filter: Filter): Delete =
    copy(filter = this.filter and filter)
}
