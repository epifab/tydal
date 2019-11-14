package io.epifab.tydal.utils

import io.epifab.tydal._
import shapeless.{::, Generic, HList}

import scala.annotation.implicitNotFound

@implicitNotFound("Field or relation ${T} could not be found")
trait TaggedFinder[T <: Tag, +X, HAYSTACK] {
  def find(u: HAYSTACK): X AS T
}

object TaggedFinder {
  implicit def tuple2Finder1[T <: Tag, X, X1, X2]
      (implicit t1Finder: TaggedFinder[T, X, X1])
      : TaggedFinder[T, X, (X1, X2)] =
    (u: (X1, X2)) => t1Finder.find(u._1)

  implicit def tuple2Finder2[T <: Tag, X, X1, X2]
      (implicit t2Finder: TaggedFinder[T, X, X2])
      : TaggedFinder[T, X, (X1, X2)] =
    (u: (X1, X2)) => t2Finder.find(u._2)

  implicit def tuple3Finder1[T <: Tag, X, X1, X2, X3]
      (implicit t1Finder: TaggedFinder[T, X, X1])
      : TaggedFinder[T, X, (X1, X2, X3)] =
    (u: (X1, X2, X3)) => t1Finder.find(u._1)

  implicit def tuple3Finder2[T <: Tag, X, X1, X2, X3]
      (implicit t2Finder: TaggedFinder[T, X, X2])
      : TaggedFinder[T, X, (X1, X2, X3)] =
    (u: (X1, X2, X3)) => t2Finder.find(u._2)

  implicit def tuple3Finder3[T <: Tag, X, X1, X2, X3]
      (implicit t3Finder: TaggedFinder[T, X, X3])
      : TaggedFinder[T, X, (X1, X2, X3)] =
    (u: (X1, X2, X3)) => t3Finder.find(u._3)

  implicit def headFinder[T <: Tag, X, TAIL <: HList]: TaggedFinder[T, X, (X AS T) :: TAIL] =
    (u: (X AS T) :: TAIL) => u.head

  implicit def tailFinder[T <: Tag, X, H, TAIL <: HList](implicit finder: TaggedFinder[T, X, TAIL]): TaggedFinder[T, X, H :: TAIL] =
    (u: H :: TAIL) => finder.find(u.tail)

  implicit def caseClassFinder[T <: Tag, X, CC, REPR]
      (implicit generic: Generic.Aux[CC, REPR],
       finder: TaggedFinder[T, X, REPR]): TaggedFinder[T, X, CC] =
    (cc: CC) => finder.find(generic.to(cc))
}
