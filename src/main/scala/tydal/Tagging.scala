package tydal

import shapeless.{::, HList, HNil}

import scala.annotation.{implicitAmbiguous, implicitNotFound}

trait Tagging[A <: String with Singleton] {
  def tagValue: String
}

trait Tagged[-F, A <: String with Singleton] {
  def tag: A
}

object Tagged {
  implicit def pure[A <: String with Singleton](implicit valueOf: ValueOf[A]): Tagged[Tagging[A], A] = new Tagged[Tagging[A], A] {
    override def tag: A = valueOf.value
  }
}

sealed trait Untagged[-F]

object Untagged {
  implicit def alwaysTagged[F, A]: Untagged[F] = new Untagged[F] { }
  @implicitAmbiguous("${F} is actually tagged")
  implicit def tagged[F, A <: String with Singleton](implicit tagged: Tagged[F, A]): Untagged[F] = new Untagged[F] {}
}

@implicitNotFound("Cannot build a tagged list of ${Needle} from ${Haystack}")
trait TagMap[+Needle, Haystack] {
  def toMap(list: Haystack): Map[String, Needle]
}

object TagMap {
  implicit def pure[Needle <: Tagging[_]]: TagMap[Needle, Needle] =
    (x: Needle) => Map(x.tagValue -> x)

  implicit def hNil[Needle]: TagMap[Needle, HNil] =
    (_: HNil) => Map.empty

  implicit def hCons[Needle, H, T <: HList]
      (implicit
       headTagMap: TagMap[Needle, H],
       tailTagMap: TagMap[Needle, T]): TagMap[Needle, H :: T] =
    (list: H :: T) => headTagMap.toMap(list.head) ++ tailTagMap.toMap(list.tail)

  def apply[Needle, Haystack](haystack: Haystack)(implicit tagMap: TagMap[Needle, Haystack]): Map[String, Needle] =
    tagMap.toMap(haystack)
}
