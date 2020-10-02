package tydal.utils

import tydal._
import shapeless.{::, HList}

import scala.annotation.implicitNotFound

@implicitNotFound("Field or relation ${T} could not be found")
trait TaggedReplace[A <: String with Singleton, -X <: Tagging[_], Haystack, NewHaystack] {
  def apply(u: Haystack, x: X): NewHaystack
}

object TaggedReplace {
  implicit def headReplace[A <: String with Singleton, X <: Tagging[_], Head, Tail <: HList](
    implicit
    tagged: Tagged[X, A]
  ): TaggedReplace[A, X, (Head As A) :: Tail, X :: Tail] =
    (u: (Head As A) :: Tail, x: X) => x :: u.tail

  implicit def tailReplace[A <: String with Singleton, X <: Tagging[_], Head, Tail <: HList, NewTail <: HList](
    implicit
    replace: TaggedReplace[A, X, Tail, NewTail]
  ): TaggedReplace[A, X, Head :: Tail, Head :: NewTail] =
    (u: Head :: Tail, x: X) => u.head :: replace(u.tail, x)
}
