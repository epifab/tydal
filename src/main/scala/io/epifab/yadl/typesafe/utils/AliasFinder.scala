package io.epifab.yadl.typesafe.utils

import io.epifab.yadl.typesafe.{AS, DataSource, Join}
import shapeless.{::, HList}

trait AliasFinder[ALIAS, X, HAYSTACK] {
  def find(u: HAYSTACK): X AS ALIAS
}

object AliasFinder {
  implicit def headFinder[ALIAS, X, T <: HList]: AliasFinder[ALIAS, X, (X AS ALIAS) :: T] =
    (u: (X AS ALIAS) :: T) => u.head

  implicit def tailFinder[ALIAS, X, H, T <: HList](implicit finder: AliasFinder[ALIAS, X, T]): AliasFinder[ALIAS, X, H :: T] =
    (u: H :: T) => finder.find(u.tail)
}

class FindByAlias[ALIAS, HAYSTACK](haystack: HAYSTACK) {
  def get[X](implicit finder: AliasFinder[ALIAS, X, HAYSTACK]): X AS ALIAS =
    finder.find(haystack)
}
