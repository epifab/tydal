package io.epifab.yadl.typesafe.utils

import shapeless.{::, HList, HNil}

trait BoundedList[+X, L <: HList] {
  def toSeq(l: L): Seq[X]
}

object BoundedList {
  class HListToSeq[X] {
    def get[L <: HList](l: L)(implicit boundedList: BoundedList[X, L]): Seq[X] =
      boundedList.toSeq(l)
  }

  def apply[X]: HListToSeq[X] = new HListToSeq[X]

  def apply[X, L <: HList](l: L)(implicit boundedList: BoundedList[X, L]): Seq[X] =
    boundedList.toSeq(l)

  implicit def boundedHNil[X]: BoundedList[X, HNil] =
    (_: HNil) => Seq.empty[Nothing]

  implicit def boundedHCons[X, T <: HList](implicit boundedTail: BoundedList[X, T]): BoundedList[X, X :: T] =
    (list: X :: T) => boundedTail.toSeq(list.tail) :+ list.head
}

