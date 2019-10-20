package io.epifab.yadl.examples

import io.epifab.yadl.examples.SelectsQueries.studentsQuery
import io.epifab.yadl.typesafe.Tag
import io.epifab.yadl.typesafe.fields._
import io.epifab.yadl.typesafe.utils.Finder
import shapeless.{::, HList, HNil}

import scala.annotation.implicitNotFound

@implicitNotFound("Not all the placeholders have been resolved")
trait EnsurePlaceholderValues[-PLACEHOLDERS, VALUES]

object EnsurePlaceholderValues {
  def instance[PS, VS]: EnsurePlaceholderValues[PS, VS] = new EnsurePlaceholderValues[PS, VS] {}

  def apply[PS, VS](ps: PS, vs: VS)(implicit r: EnsurePlaceholderValues[PS, VS]): Unit = { }

  implicit def singleResolved[X, NAME <: String, VS](implicit finder: Finder[PlaceholderValue[X] with Tag[NAME], VS]):
    EnsurePlaceholderValues[Placeholder[X, X] with Tag[NAME], VS] = instance

  implicit def hNil[VS]:
    EnsurePlaceholderValues[HNil, VS] = instance

  implicit def hCons[H, T <: HList, VS](implicit r1: EnsurePlaceholderValues[H, VS], r2: EnsurePlaceholderValues[T, VS]):
    EnsurePlaceholderValues[H :: T, VS] = instance
}

object TestingPlaceholder {
  private val a = Placeholder["A", Int]
  private val b = Placeholder["B", String]
  private val ab = a :: b :: HNil

  private val values = a.resolve(12) :: b.resolve("test") :: HNil

  EnsurePlaceholderValues(ab, values)
  EnsurePlaceholderValues(a :: ab, values)

  val x: Column[Int] with Tag["sid"] ::
    Column[String] with Tag["sname"] ::
    Column[Option[Int]] with Tag["score"] ::
    Nullable[Column[String], String] with Tag["cname"] ::
    HNil = studentsQuery.fields

  PlaceholderExtractor(studentsQuery)
  PlaceholderExtractor(studentsQuery.filter)
  PlaceholderExtractor(studentsQuery.fields)
}
