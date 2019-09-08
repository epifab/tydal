package io.epifab.yadl.examples

import io.epifab.yadl.typesafe.Implicits._
import io.epifab.yadl.typesafe._
import io.epifab.yadl.typesafe.fields.{Aggregation, Column, Max, Min, Placeholder}
import shapeless.{::, HNil}

object TypesafeSchema {
  object Students extends TableBuilder[
    "students",
      (Column[Int] AS "id") ::
      (Column[String] AS "name") ::
      HNil
  ]

  object Exams extends TableBuilder[
    "exams",
      (Column[Int] AS "student_id") ::
      (Column[Int] AS "course_id") ::
      (Column[Int] AS "score") ::
      HNil
  ]

  object Courses extends TableBuilder[
    "courses",
      (Column[Int] AS "id") ::
      (Column[String] AS "name") ::
      HNil
  ]

  def maxScoreSubQuery[E <: String](implicit eAlias: ValueOf[E]) =
    Select
      .from(Exams.as[E])
      .groupBy(ctx => ctx[E, "student_id"].get :: HNil)
      .take(ctx =>
        ctx[E, "student_id"].get ::
        Max(ctx[E, "score"].get).as["max_score"] ::
        Min(ctx[E, "course_id"].get).as["course_id"] ::
        HNil)

  val studentsSelect =
    Select
      .from(Students.as["s"])
      .join(ctx => maxScoreSubQuery["e"].as["ms"]
        .on(_["student_id"].get === ctx["s", "id"].get))
      .join(ctx => Courses.as["cc"].on(_["id"].get === ctx["ms", "course_id"].get))
      .take(ctx =>
        ctx["s", "id"].get.as["sid"] ::
        ctx["s", "name"].get.as["sname"] ::
        ctx["ms", "max_score"].get.as["score"] ::
        ctx["cc", "name"].get.as["cname"] ::
        HNil
      )
      .withPlaceholder[Int, "student_id"]
      .where(ctx => ctx["s", "id"].get === ctx["student_id"].get)
}
