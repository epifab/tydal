package io.epifab.yadl.typesafe

import shapeless.{::, HList, HNil}

trait Tag[A <: String] {
  def tagValue: String
}

trait Tagged[+NEEDLE, HAYSTACK] {
  def toMap(list: HAYSTACK): Map[String, NEEDLE]
}

object Tagged {
  implicit def pure[NEEDLE <: Tag[_]]: Tagged[NEEDLE, NEEDLE] =
    (x: NEEDLE) => Map(x.tagValue -> x)

  implicit def hNil[NEEDLE]: Tagged[NEEDLE, HNil] =
    (_: HNil) => Map.empty

  implicit def hCons[NEEDLE, H, T <: HList]
      (implicit
       headTagMap: Tagged[NEEDLE, H],
       tailTagMap: Tagged[NEEDLE, T]): Tagged[NEEDLE, H :: T] =
    (list: H :: T) => headTagMap.toMap(list.head) ++ tailTagMap.toMap(list.tail)

  def apply[NEEDLE, HAYSTACK](haystack: HAYSTACK)(implicit tagMap: Tagged[NEEDLE, HAYSTACK]): Map[String, NEEDLE] =
    tagMap.toMap(haystack)
}
