package io.epifab.dal.domain

trait Delete {
  def dataSource: DataSource
  def filter: Filter

  def where(filter: Filter): Delete
}

object Delete {
  protected case class DeleteImpl(dataSource: DataSource, filter: Filter = Filter.Empty) extends Delete {
    def where(filter: Filter): Delete =
      copy(filter = this.filter and filter)
  }

  def apply(dataSource: DataSource) = DeleteImpl(dataSource)
}

