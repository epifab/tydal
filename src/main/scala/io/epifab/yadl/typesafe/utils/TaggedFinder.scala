package io.epifab.yadl.typesafe.utils

import io.epifab.yadl.typesafe.{AS, DataSource, Join}
import shapeless.{::, HList}

trait TaggedFinder[TAG, X, HAYSTACK] {
  def find(u: HAYSTACK): X AS TAG
}

object TaggedFinder {
  implicit def headFinder[TAG, X, T <: HList]: TaggedFinder[TAG, X, (X AS TAG) :: T] =
    (u: (X AS TAG) :: T) => u.head

  implicit def tailFinder[TAG, X, H, T <: HList](implicit finder: TaggedFinder[TAG, X, T]): TaggedFinder[TAG, X, H :: T] =
    (u: H :: T) => finder.find(u.tail)

  implicit def joinFinder[TAG, X <: DataSource[_], T <: HList]: TaggedFinder[TAG, X, Join[X AS TAG] :: T] =
    (u: Join[X AS TAG] :: T) => u.head.dataSource
}

class FindByTag[TAG, HAYSTACK](haystack: HAYSTACK) {
  def get[X](implicit finder: TaggedFinder[TAG, X, HAYSTACK]): X AS TAG =
    finder.find(haystack)
}
