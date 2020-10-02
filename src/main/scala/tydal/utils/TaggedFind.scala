package tydal.utils

import tydal._
import shapeless.{::, HList}

import scala.annotation.implicitNotFound

@implicitNotFound("Field or relation ${A} could not be found")
trait TaggedFind[A <: String with Singleton, +X, -Haystack] {
  def apply(u: Haystack): X As A
}

object TaggedFind {
  implicit def tuple2Find1[T <: String with Singleton, X, X1, X2](
    implicit
    t1Find: TaggedFind[T, X, X1]
  ): TaggedFind[T, X, (X1, X2)] =
    (u: (X1, X2)) => t1Find(u._1)

  implicit def tuple2Find2[T <: String with Singleton, X, X1, X2](
    implicit
    t2Find: TaggedFind[T, X, X2]
  ): TaggedFind[T, X, (X1, X2)] =
    (u: (X1, X2)) => t2Find(u._2)

  implicit def headFind[T <: String with Singleton, X, Tail <: HList]: TaggedFind[T, X, (X As T) :: Tail] =
    (u: (X As T) :: Tail) => u.head

  implicit def tailFind[T <: String with Singleton, X, H, Tail <: HList](
    implicit
    find: TaggedFind[T, X, Tail]
  ): TaggedFind[T, X, H :: Tail] =
    (u: H :: Tail) => find(u.tail)
}
