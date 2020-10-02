package tydal.utils

import tydal._
import shapeless.{::, HList}

import scala.annotation.implicitNotFound

@implicitNotFound("Field or relation ${T} could not be found")
trait TaggedRemove[A <: String with Singleton, Haystack, NewHaystack] {
  def apply(u: Haystack): NewHaystack
}

object TaggedRemove {
  implicit def headRemove[A <: String with Singleton, Head, Tail <: HList]: TaggedRemove[A, (Head As A) :: Tail, Tail] =
    (u: (Head As A) :: Tail) => u.tail

  implicit def tailRemove[A <: String with Singleton, Head, Tail <: HList, NewTail <: HList](
    implicit
    replace: TaggedRemove[A, Tail, NewTail]
  ): TaggedRemove[A, Head :: Tail, Head :: NewTail] =
    (u: Head :: Tail) => u.head :: replace(u.tail)
}
