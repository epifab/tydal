package io.epifab.yadl.typesafe

import shapeless.{::, HList, HNil}

trait Tag[A]

trait Taggable {
  def as[TAG]: this.type with Tag[TAG] =
    this.asInstanceOf[this.type with Tag[TAG]]
}

trait TagMap[+NEEDLE, HAYSTACK] {
  def toMap(list: HAYSTACK): Map[String, NEEDLE]
}

object TagMap {
  trait TagValue[-X <: Tag[_]] {
    def getTag(x: X): String
  }

  object TagValue {
    implicit def pure[TAG <: String](implicit tag: ValueOf[TAG]): TagValue[Tag[TAG]] =
      (_: Tag[TAG]) => tag.value
  }

  implicit def pure[NEEDLE <: Tag[_]](implicit tagValue: TagValue[NEEDLE]): TagMap[NEEDLE, NEEDLE] =
    (x: NEEDLE) => Map(tagValue.getTag(x) -> x)

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
