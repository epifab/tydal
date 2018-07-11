package io.epifab.yadl.domain

sealed trait Sort {
  def column: Column[_]
}

final case class AscSort(column: Column[_]) extends Sort
final case class DescSort(column: Column[_]) extends Sort
