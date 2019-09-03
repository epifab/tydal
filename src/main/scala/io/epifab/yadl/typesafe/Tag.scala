package io.epifab.yadl.typesafe

import shapeless.{::, HList, HNil}

trait Tag[A <: String] {
  def tagValue: String
}

trait TagMap[+NEEDLE, HAYSTACK] {
  def toMap(list: HAYSTACK): Map[String, NEEDLE]
}

object TagMap {
  implicit def pure[NEEDLE <: Tag[_]]: TagMap[NEEDLE, NEEDLE] =
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
