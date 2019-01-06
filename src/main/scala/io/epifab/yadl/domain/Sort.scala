package io.epifab.yadl.domain

sealed trait Sort {
  def column: Field[_]
}

final case class AscSort(column: Field[_]) extends Sort
final case class DescSort(column: Field[_]) extends Sort
