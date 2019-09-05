package io.epifab.yadl.examples

import io.epifab.yadl.typesafe.Implicits._
import io.epifab.yadl.typesafe._
import io.epifab.yadl.typesafe.fields.{Column, Max, Min}
import shapeless.{::, HNil}

object TypesafeSchema {
  class Students extends Table[
    "students",
      (Column[Int] AS "id") ::
      (Column[String] AS "name") ::
      HNil
  ]

  object Students {
    def as[T <: String](implicit alias: ValueOf[T]): Students AS T =
      new Students with Tag[T] {
        override def tagValue: String = alias.value
      }
  }

  class Exams extends Table[
    "exams",
      (Column[Int] AS "student_id") ::
      (Column[Int] AS "course_id") ::
      (Column[Int] AS "score") ::
      HNil
  ]

  object Exams {
    def as[T <: String](implicit alias: ValueOf[T]): Exams AS T = new Exams with Tag[T] {
      override def tagValue: String = alias.value
    }
  }

  class Courses extends Table[
    "courses",
      (Column[Int] AS "id") ::
      (Column[String] AS "name") ::
      HNil
  ]

  object Courses {
    def as[T <: String](implicit alias: ValueOf[T]): Courses AS T = new Courses with Tag[T] {
      override def tagValue: String = alias.value
    }
  }

  def maxScoreSubQuery[E <: String](implicit eAlias: ValueOf[E]) =
    Select
      .from(Exams.as[E])
      .groupBy(ctx => ctx[E].get.apply["student_id"].get :: HNil)
      .take(ctx =>
        ctx[E].get.apply["student_id"].get ::
        Max(ctx[E].get.apply["score"].get).as["max_score"] ::
        Min(ctx[E].get.apply["course_id"].get).as["course_id"] ::
        HNil)

  val studentsSelect =
    Select
      .from(Students.as["s"])
      .join(ctx => maxScoreSubQuery["e"].as["ms"]
        .on(_["student_id"].get === ctx["s"].get.apply["id"].get))
      .join(ctx => Courses.as["cc"].on(_["id"].get ===
        ctx["ms"].get.apply["course_id"].get))
      .take(ctx =>
        ctx["s"].get.apply["id"].get.as["sid"] ::
        ctx["s"].get.apply["name"].get.as["sname"] ::
        ctx["ms"].get.apply["max_score"].get.as["score"] ::
        ctx["cc"].get.apply["name"].get.as["cname"] ::
        HNil
      )
      .withPlaceholder[Int, "student_id"]
      .where(ctx => ctx["s"].get.apply["id"].get === ctx["student_id"].get)
}
