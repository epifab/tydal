package io.epifab.yadl.domain

sealed trait Sort {
  def field: Field[_]
}

final case class AscSort(field: Field[_]) extends Sort
final case class DescSort(field: Field[_]) extends Sort
