package io.epifab.yadl.domain

sealed trait Sort {
  def term: Term[_]
}

final case class AscSort(term: Term[_]) extends Sort
final case class DescSort(term: Term[_]) extends Sort
