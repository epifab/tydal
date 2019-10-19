package io.epifab.yadl.typesafe.utils

import io.epifab.yadl.typesafe.fields.BinaryExpr
import io.epifab.yadl.typesafe.{AS, DataSource, FindContext, Join}
import shapeless.{::, Generic, HList}

trait TaggedFinder[TAG <: String, +X, HAYSTACK] {
  def find(u: HAYSTACK): X AS TAG
}

object TaggedFinder {
  implicit def tuple2Finder1[TAG <: String, X, T1, T2]
      (implicit t1Finder: TaggedFinder[TAG, X, T1])
      : TaggedFinder[TAG, X, (T1, T2)] =
    (u: (T1, T2)) => t1Finder.find(u._1)

  implicit def tuple2Finder2[TAG <: String, X, T1, T2]
      (implicit t2Finder: TaggedFinder[TAG, X, T2])
      : TaggedFinder[TAG, X, (T1, T2)] =
    (u: (T1, T2)) => t2Finder.find(u._2)

  implicit def tuple3Finder1[TAG <: String, X, T1, T2, T3]
      (implicit t1Finder: TaggedFinder[TAG, X, T1])
      : TaggedFinder[TAG, X, (T1, T2, T3)] =
    (u: (T1, T2, T3)) => t1Finder.find(u._1)

  implicit def tuple3Finder2[TAG <: String, X, T1, T2, T3]
      (implicit t2Finder: TaggedFinder[TAG, X, T2])
      : TaggedFinder[TAG, X, (T1, T2, T3)] =
    (u: (T1, T2, T3)) => t2Finder.find(u._2)

  implicit def tuple3Finder3[TAG <: String, X, T1, T2, T3]
      (implicit t3Finder: TaggedFinder[TAG, X, T3])
      : TaggedFinder[TAG, X, (T1, T2, T3)] =
    (u: (T1, T2, T3)) => t3Finder.find(u._3)

  implicit def headFinder[TAG <: String, X, T <: HList]: TaggedFinder[TAG, X, (X AS TAG) :: T] =
    (u: (X AS TAG) :: T) => u.head

  implicit def tailFinder[TAG <: String, X, H, T <: HList](implicit finder: TaggedFinder[TAG, X, T]): TaggedFinder[TAG, X, H :: T] =
    (u: H :: T) => finder.find(u.tail)

  implicit def caseClassFinder[TAG <: String, X, CC, REPR]
      (implicit generic: Generic.Aux[CC, REPR],
       finder: TaggedFinder[TAG, X, REPR]): TaggedFinder[TAG, X, CC] =
    (cc: CC) => finder.find(generic.to(cc))

  implicit def joinFinder[TAG <: String, X <: DataSource[_], E <: BinaryExpr, T <: HList]: TaggedFinder[TAG, X, Join[X AS TAG, E] :: T] =
    (u: Join[X AS TAG, E] :: T) => u.head.dataSource
}

class FindByTag[TAG <: String, HAYSTACK](haystack: HAYSTACK) {
  def get[X](implicit finder: TaggedFinder[TAG, X, HAYSTACK]): X AS TAG =
    finder.find(haystack)
}

class FindNestedByTag[TAG1 <: String, TAG2 <: String, HAYSTACK1](haystack: HAYSTACK1)(implicit tag2: ValueOf[TAG2]) {
  def get[HAYSTACK2, X2]
      (implicit
       finder1: TaggedFinder[TAG1, FindContext[HAYSTACK2], HAYSTACK1],
       finder2: TaggedFinder[TAG2, X2, HAYSTACK2]): X2 AS TAG2 =
    finder1.find(haystack).apply[TAG2].get[X2]
}
