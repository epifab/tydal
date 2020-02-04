package io.epifab.tydal.utils

import io.epifab.tydal.Tagging
import shapeless.{::, HList, HNil}


sealed trait HSet[Source <: HList, Target <: HList] {
  def toSet(list: Source): Target
  def toList(set: Target): Source
}

object HSet {

  sealed trait RemoveElement[Needle, Haystack, NeedleFreeHaystack <: HList] {
    def remove(haystack: Haystack): NeedleFreeHaystack
    def undo(needle: Needle, needleFreeHaystack: NeedleFreeHaystack): Haystack
  }

  object RemoveElement {
    class PartiallyAppliedRemoveElement[Needle] {
      def apply[Haystack, ResultSet <: HList](b: Haystack)(implicit removeByTag: RemoveElement[Needle, Haystack, ResultSet]): ResultSet =
        removeByTag.remove(b)
    }

    def apply[Needle]: PartiallyAppliedRemoveElement[Needle] = new PartiallyAppliedRemoveElement[Needle]

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
      override def undo(needle: A, needleFreeHaystack: HNil): B = needle.asInstanceOf[B]
    }

    implicit def notRemoved[A <: Tagging[_], B <: Tagging[_]](implicit differentElement: DifferentElement[A, B]): RemoveElement[A, B, B :: HNil] = new RemoveElement[A, B, B :: HNil] {
      override def remove(haystack: B): B :: HNil = haystack :: HNil
      override def undo(needle: A, needleFreeHaystack: B :: HNil): B = needleFreeHaystack.head
    }

    implicit def hNil[A]: RemoveElement[A, HNil, HNil] = new RemoveElement[A, HNil, HNil] {
      override def remove(haystack: HNil): HNil = HNil
      override def undo(needle: A, needleFreeHaystack: HNil): HNil = HNil
    }

    implicit def hCons[A, H, HX <: HList, T <: HList, TX <: HList, XX <: HList]
    (implicit
     headRemover: RemoveElement[A, H, HX],
     tailRemover: RemoveElement[A, T, TX],
     concat: Concat.Aux[HX, TX, XX]): RemoveElement[A, H :: T, XX] = new RemoveElement[A, H :: T, XX] {
      override def remove(haystack: H :: T): XX =
        concat(headRemover.remove(haystack.head), tailRemover.remove(haystack.tail))

      override def undo(needle: A, needleFreeHaystack: XX): H :: T =
        concat.undo(needleFreeHaystack) match {
          case (hx, tx) => headRemover.undo(needle, hx) :: tailRemover.undo(needle, tx)
        }
    }
  }

  class PartiallyAppliedHSet[Source <: HList](src: Source) {
    def get[Target <: HList](implicit hSet: HSet[Source, Target]): Target =
      hSet.toSet(src)
  }

  def apply[Source <: HList](src: Source): PartiallyAppliedHSet[Source] = new PartiallyAppliedHSet(src)

  private def instance[A <: HList, B <: HList](f: A => B, g: B => A): HSet[A, B] = new HSet[A, B] {
    override def toSet(list: A): B = f(list)
    override def toList(set: B): A = g(set)
  }

  implicit def hNil: HSet[HNil, HNil] =
    instance((_: HNil) => HNil, (_: HNil) => HNil)

  implicit def hCons[H <: Tagging[_], T <: HList, HX <: HList, XX <: HList](
    implicit
    removeHeadFromTail: RemoveElement[H, T, HX],
    nestedHSet: HSet[HX, XX]
  ): HSet[H :: T, H :: XX] =
    instance(
      (list: H :: T) => list.head :: nestedHSet.toSet(removeHeadFromTail.remove(list.tail)),
      (set: H :: XX) => set.head :: removeHeadFromTail.undo(set.head, nestedHSet.toList(set.tail))
    )
}
