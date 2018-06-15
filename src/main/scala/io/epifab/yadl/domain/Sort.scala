package io.epifab.yadl.domain

sealed trait Sort {
  def source: DataSource
}

final case class AscSort(source: DataSource) extends Sort
final case class DescSort(source: DataSource) extends Sort
