package io.epifab.yadl.examples

import io.epifab.yadl.typesafe.Implicits._
import io.epifab.yadl.typesafe._
import shapeless.{::, HNil}

object TypesafeSchema {
  class Students extends Table[
    "students",
      (Field[Int] AS "id") ::
      (Field[String] AS "name") ::
      HNil
  ]

  object Students {
    def as[T]: Students AS T = new Students with Tag[T]
  }

  class Exams extends Table[
    "exams",
      (Field[Int] AS "student_id") ::
      (Field[Int] AS "course_id") ::
      (Field[Int] AS "score") ::
      HNil
  ]

  object Exams {
    def as[T]: Exams AS T = new Exams with Tag[T]
  }

  class Courses extends Table[
    "courses",
      (Field[Int] AS "id") ::
      (Field[String] AS "name") ::
      HNil
  ]

  object Courses {
    def as[T]: Courses AS T = new Courses with Tag[T]
  }

  def maxScoreSubQuery[E] =
    Select
      .from(Exams.as[E])
      .groupBy(ctx => ctx.source[E].get.field["student_id"].get :: HNil)
      .take(ctx =>
        ctx.source[E].get.field["student_id"].get ::
        Max(ctx.source[E].get.field["score"].get).as["max_score"] ::
        Min(ctx.source[E].get.field["course_id"].get).as["course_id"] ::
        HNil)

  val studentsSelect =
    Select
      .from(Students.as["s"])
      .join(ctx => maxScoreSubQuery["e"].as["ms"]
        .on(_.field["student_id"].get === ctx.source["s"].get.field["id"].get))
      .join(ctx => Courses.as["cc"].on(_.field["id"].get ===
        ctx.source["ms"].get.field["course_id"].get))
      .take(ctx =>
        ctx.source["s"].get.field["id"].get.as["sid"] ::
        ctx.source["s"].get.field["name"].get.as["sname"] ::
        ctx.source["ms"].get.field["max_score"].get.as["score"] ::
        ctx.source["cc"].get.field["name"].get.as["cname"] ::
        HNil
      )
      .withPlaceholder[Int, "student_id"]
      .where(ctx => ctx.source["s"].get.field["id"].get === ctx.placeholder["student_id"].get)
}
