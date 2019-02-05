package io.epifab.yadl.domain

sealed trait Sort {
  def term: Term[_]
}

final case class Asc(term: Term[_]) extends Sort
final case class Desc(term: Term[_]) extends Sort
