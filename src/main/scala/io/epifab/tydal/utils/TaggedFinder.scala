package io.epifab.tydal.utils

import io.epifab.tydal._
import shapeless.{::, Generic, HList}

import scala.annotation.implicitNotFound

@implicitNotFound("Field or relation ${T} could not be found")
trait TaggedFinder[T <: String with Singleton, +X, Haystack] {
  def find(u: Haystack): X As T
}

object TaggedFinder {
  implicit def tuple2Finder1[T <: String with Singleton, X, X1, X2]
      (implicit t1Finder: TaggedFinder[T, X, X1])
      : TaggedFinder[T, X, (X1, X2)] =
    (u: (X1, X2)) => t1Finder.find(u._1)

  implicit def tuple2Finder2[T <: String with Singleton, X, X1, X2]
      (implicit t2Finder: TaggedFinder[T, X, X2])
      : TaggedFinder[T, X, (X1, X2)] =
    (u: (X1, X2)) => t2Finder.find(u._2)

  implicit def tuple3Finder1[T <: String with Singleton, X, X1, X2, X3]
      (implicit t1Finder: TaggedFinder[T, X, X1])
      : TaggedFinder[T, X, (X1, X2, X3)] =
    (u: (X1, X2, X3)) => t1Finder.find(u._1)

  implicit def tuple3Finder2[T <: String with Singleton, X, X1, X2, X3]
      (implicit t2Finder: TaggedFinder[T, X, X2])
      : TaggedFinder[T, X, (X1, X2, X3)] =
    (u: (X1, X2, X3)) => t2Finder.find(u._2)

  implicit def tuple3Finder3[T <: String with Singleton, X, X1, X2, X3]
      (implicit t3Finder: TaggedFinder[T, X, X3])
      : TaggedFinder[T, X, (X1, X2, X3)] =
    (u: (X1, X2, X3)) => t3Finder.find(u._3)

  implicit def headFinder[T <: String with Singleton, X, Tail <: HList]: TaggedFinder[T, X, (X As T) :: Tail] =
    (u: (X As T) :: Tail) => u.head

  implicit def tailFinder[T <: String with Singleton, X, H, Tail <: HList]
      (implicit finder: TaggedFinder[T, X, Tail])
      : TaggedFinder[T, X, H :: Tail] =
    (u: H :: Tail) => finder.find(u.tail)

  implicit def caseClassFinder[T <: String with Singleton, X, CC, Repr]
      (implicit generic: Generic.Aux[CC, Repr],
       finder: TaggedFinder[T, X, Repr])
      : TaggedFinder[T, X, CC] =
    (cc: CC) => finder.find(generic.to(cc))
}
