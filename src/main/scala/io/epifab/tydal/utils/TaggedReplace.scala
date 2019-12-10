package io.epifab.tydal.utils

import io.epifab.tydal._
import shapeless.{::, HList}

import scala.annotation.implicitNotFound

@implicitNotFound("Field or relation ${T} could not be found")
trait TaggedReplace[A <: String with Singleton, X, Haystack, NewHaystack] {
  def apply(u: Haystack, x: X As A): NewHaystack
}

object TaggedReplace {
  implicit def headReplace[A <: String with Singleton, X, Head, Tail <: HList]: TaggedReplace[A, X, (Head As A) :: Tail, (X As A) :: Tail] =
    (u: (Head As A) :: Tail, x: X As A) => x :: u.tail

  implicit def tailReplace[A <: String with Singleton, X, Head, Tail <: HList, NewTail <: HList](
    implicit
    replace: TaggedReplace[A, X, Tail, NewTail]
  ): TaggedReplace[A, X, Head :: Tail, Head :: NewTail] =
    (u: Head :: Tail, x: X As A) => u.head :: replace(u.tail, x)
}
