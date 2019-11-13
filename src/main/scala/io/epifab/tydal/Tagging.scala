package io.epifab.tydal

import shapeless.{::, HList, HNil}

import scala.annotation.{implicitAmbiguous, implicitNotFound}

trait Tagging[A <: Tag] {
  def tagValue: String
}

trait Tagged[-F, A <: Tag] {
  def tag: A
}

object Tagged {
  implicit def pure[A <: Tag](implicit valueOf: ValueOf[A]): Tagged[Tagging[A], A] = new Tagged[Tagging[A], A] {
    override def tag: A = valueOf.value
  }
}

@implicitNotFound("Cannot build a tagged list of ${NEEDLE} from ${HAYSTACK}")
trait TagMap[+NEEDLE, HAYSTACK] {
  def toMap(list: HAYSTACK): Map[String, NEEDLE]
}

object TagMap {
  implicit def pure[NEEDLE <: Tagging[_]]: TagMap[NEEDLE, NEEDLE] =
    (x: NEEDLE) => Map(x.tagValue -> x)

  implicit def hNil[NEEDLE]: TagMap[NEEDLE, HNil] =
    (_: HNil) => Map.empty

  implicit def hCons[NEEDLE, H, T <: HList]
      (implicit
       headTagMap: TagMap[NEEDLE, H],
       tailTagMap: TagMap[NEEDLE, T]): TagMap[NEEDLE, H :: T] =
    (list: H :: T) => headTagMap.toMap(list.head) ++ tailTagMap.toMap(list.tail)

  def apply[NEEDLE, HAYSTACK](haystack: HAYSTACK)(implicit tagMap: TagMap[NEEDLE, HAYSTACK]): Map[String, NEEDLE] =
    tagMap.toMap(haystack)
}
