package io.epifab.yadl.utils

import shapeless.{::, HList, HNil}

private trait Finder[X, U] {
  def find(u: U): X
}

private object Finder {
  implicit def headFinder[X, T <: HList]: Finder[X, X :: T] =
    (u: X :: T) => u.head

  implicit def tailFinder[X, H, T <: HList](implicit finder: Finder[X, T]): Finder[X, H :: T] =
    (u: H :: T) => finder.find(u.tail)
}

private trait Concat[T, U] {
  type Out
  def concat(t: T, u: U): Out
}

object Concat {
  type Aux[T, U, O] = Concat[T, U] { type Out = O }

  implicit def concatHNil[L <: HList]: Concat.Aux[HNil, L, L] = new Concat[HNil, L] {
    override type Out = L
    override def concat(h: HNil, l: L): Out = l
  }

  implicit def concatHCons[H, T <: HList, L <: HList, O <: HList](implicit tailConcat: Concat.Aux[T, L, O]): Concat.Aux[H :: T, L, H :: O] = new Concat[H :: T, L] {
    override type Out = H :: O
    override def concat(t: H :: T, u: L): Out = t.head :: tailConcat.concat(t.tail, u)
  }
}

trait ReverseAppender[T, U] {
  type Out
  def appendReverse(t: T, u: U): Out
}

trait Appender[L, X] {
  type Out
  def append(l: L, x: X): Out
}

object Appender {
  type Aux[L, X, O] = Appender[L, X] { type Out = O }

  implicit def hNilAppender[X]: Appender.Aux[HNil, X, X :: HNil] = new Appender[HNil, X] {
    override type Out = X :: HNil
    override def append(l: HNil, x: X): X :: HNil = x :: HNil
  }

  implicit def hConsAppender[H, T <: HList, X, TAO <: HList](implicit tailAppender: Appender.Aux[T, X, TAO]): Appender.Aux[H :: T, X, H :: TAO] = new Appender[H :: T, X] {
    override type Out = H :: TAO
    override def append(t: H :: T, x: X): Out = t.head :: tailAppender.append(t.tail, x)
  }
}

object ReverseAppender {
  type Aux[T, U, O] = ReverseAppender[T, U] { type Out = O }

  implicit def reverseAppenderHNil[L <: HList]: ReverseAppender.Aux[HNil, L, L] = new ReverseAppender[HNil, L] {
    override type Out = L
    override def appendReverse(n: HNil, l: L): L = l
  }

  implicit def reverseAppenderHCons[H, T <: HList, L <: HList, O <: HList](implicit tailReverseAppender: ReverseAppender.Aux[T, H :: L, O]): ReverseAppender.Aux[H :: T, L, O] =
    new ReverseAppender[H :: T, L] {
      override type Out = O
      override def appendReverse(t: H :: T, u: L): Out = tailReverseAppender.appendReverse(t.tail, t.head :: u)
    }
}


object HListOps {
  def reverse[L <: HList, O](l: L)(implicit reverseAppender: ReverseAppender.Aux[L, HNil, O]): O =
    reverseAppender.appendReverse(l, HNil)

  def append[L <: HList, X, O <: HList](l: L, x: X)(implicit appender: Appender.Aux[L, X, O]): O =
    appender.append(l, x)

  def concat[L1 <: HList, L2 <: HList, O <: HList](l1: L1, l2: L2)(implicit concat: Concat.Aux[L1, L2, O]): O =
    concat.concat(l1, l2)
}
