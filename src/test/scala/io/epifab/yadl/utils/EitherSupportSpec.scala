package io.epifab.yadl.utils

import org.scalatest.{FlatSpec, Matchers}

class EitherSupportSpec extends FlatSpec with Matchers {
  import EitherSupport._

  "EitherSupport" should "build a Vector from an iterator when all elements are Right" in {
    val from = Iterable(Right(1), Right(2), Right(3))
    leftOrRights[Nothing, Int, Vector](from.iterator) shouldBe Right(Vector(1, 2, 3))
  }

  it should "build a Left from an iterator when one of the element is Left" in {
    val from = Iterable(Right(1), Left("error 1"), Left("error 2"))
    leftOrRights[String, Int, Vector](from.iterator) shouldBe Left("error 1")
  }
}
