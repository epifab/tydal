package io.epifab.tydal.fields

import io.epifab.tydal.Tagging
import io.epifab.tydal.utils.Concat
import shapeless.{::, HList, HNil}


sealed trait HSet[SRC <: HList, TARGET <: HList] {
  def toSet(src: SRC): TARGET
}

object HSet {
  sealed trait RemoveElement[NEEDLE, HAYSTACK, RESULTS <: HList] {
    def remove(haystack: HAYSTACK): RESULTS
  }

  object RemoveElement {
    class PartiallyAppliedRemoveElement[NEEDLE] {
      def apply[HAYSTACK, RESULTS <: HList](b: HAYSTACK)(implicit removeByTag: RemoveElement[NEEDLE, HAYSTACK, RESULTS]): RESULTS =
        removeByTag.remove(b)
    }

    def apply[NEEDLE]: PartiallyAppliedRemoveElement[NEEDLE] = new PartiallyAppliedRemoveElement[NEEDLE]

    sealed trait SameElement[+A, -B]

    object SameElement {
      def apply[A, B](implicit sameElement: SameElement[A, B]): Unit = {}

      implicit def sameElement[A]: SameElement[A, A] =  new SameElement[A, A] {}
    }

    trait DifferentElement[A, B]

    object DifferentElement {
      def apply[A, B](implicit differentElement: DifferentElement[A, B]): Unit = {}

      implicit def actuallyTheSame[A, B](implicit sameElement: SameElement[A, B]): DifferentElement[A, B] =
        new DifferentElement[A, B] {}

      implicit def differentElement[A, B]: DifferentElement[A, B] =
        new DifferentElement[A, B] {}
    }

    implicit def removed[A <: Tagging[_], B <: Tagging[_]](implicit sameElement: SameElement[A, B]): RemoveElement[A, B, HNil] = new RemoveElement[A, B, HNil] {
      override def remove(haystack: B): HNil = HNil
    }

    implicit def notRemoved[A <: Tagging[_], B <: Tagging[_]](implicit differentElement: DifferentElement[A, B]): RemoveElement[A, B, B :: HNil] = new RemoveElement[A, B, B :: HNil] {
      override def remove(haystack: B): B :: HNil = haystack :: HNil
    }

    implicit def hNil[A]: RemoveElement[A, HNil, HNil] = new RemoveElement[A, HNil, HNil] {
      override def remove(haystack: HNil): HNil = HNil
    }

    implicit def hCons[A, H, HX <: HList, T <: HList, TX <: HList, XX <: HList]
    (implicit
     headRemover: RemoveElement[A, H, HX],
     tailRemover: RemoveElement[A, T, TX],
     concat: Concat.Aux[HX, TX, XX]): RemoveElement[A, H :: T, XX] = new RemoveElement[A, H :: T, XX] {
      override def remove(haystack: H :: T): XX =
        concat(headRemover.remove(haystack.head), tailRemover.remove(haystack.tail))
    }
  }

  class PartiallyAppliedHSet[SRC <: HList](src: SRC) {
    def get[TARGET <: HList](implicit hSet: HSet[SRC, TARGET]): TARGET =
      hSet.toSet(src)
  }

  def apply[SRC <: HList](src: SRC): PartiallyAppliedHSet[SRC] = new PartiallyAppliedHSet(src)

  private def instance[A <: HList, B <: HList](f: A => B): HSet[A, B] = new HSet[A, B] {
    override def toSet(src: A): B = f(src)
  }

  implicit def hNil: HSet[HNil, HNil] =
    instance((_: HNil) => HNil)

  implicit def hCons[H <: Tagging[_], T <: HList, HX <: HList, XX <: HList]
    (implicit
     removeHeadFromTail: RemoveElement[H, T, HX],
     hSet2: HSet[HX, XX]): HSet[H :: T, H :: XX] =
    instance((list: H :: T) =>
      list.head :: hSet2.toSet(removeHeadFromTail.remove(list.tail)))
}
