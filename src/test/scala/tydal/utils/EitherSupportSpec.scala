package tydal.utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tydal.runtime.DataError

class EitherSupportSpec extends AnyFlatSpec with Matchers {
  import cats.implicits._
  import EitherSupport._

  "EitherSupport" should "build a Vector from an iterator when all elements are Right" in {
    val from: List[Either[DataError, Int]] = List(Right(1), Right(2), Right(3))
    leftOrRights[List, DataError, Int](from) shouldBe Right(List(1, 2, 3))
  }

  it should "build a Left from an iterator when one of the element is Left" in {
    val from: List[Either[String, Int]] = List(Right(1), Left("error 1"), Left("error 2"))
    leftOrRights[List, String, Int](from) shouldBe Left("error 1")
  }
}
