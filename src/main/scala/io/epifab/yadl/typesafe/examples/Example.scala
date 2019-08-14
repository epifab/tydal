package io.epifab.yadl.typesafe.examples

import io.epifab.yadl.typesafe.Implicits._
import io.epifab.yadl.typesafe._
import shapeless.{::, HNil}

object Example {
  class Students extends Table[
    "students",
      (Term[Int] AS "id") ::
      (Term[String] AS "name") ::
      HNil
  ]

  object Students {
    def as[T]: Students AS T = (new Students).as[T]
  }

  class Exams extends Table[
    "exams",
      (Term[Int] AS "student_id") ::
      (Term[Int] AS "course_id") ::
      (Term[Int] AS "score") ::
      HNil
  ]

  object Exams {
    def as[T]: Exams AS T = (new Exams).as[T]
  }

  class Courses extends Table[
    "courses",
      (Term[Int] AS "id") ::
      (Term[String] AS "name") ::
      HNil
  ]

  object Courses {
    def as[T]: Courses AS T = (new Courses).as[T]
  }

  def maxScoreSubQuery[E] =
    Select
      .from(Exams.as[E])
      .groupBy(ctx => ctx.source[E].get.term["student_id"].get :: HNil)
      .take(ctx =>
        ctx.source[E].get.term["student_id"].get ::
        Max(ctx.source[E].get.term["score"].get).as["max_score"] ::
        Min(ctx.source[E].get.term["course_id"].get).as["course_id"] ::
        HNil)

  val studentsSelect =
    Select
      .from(Students.as["s"])
      .join(ctx => maxScoreSubQuery["e"].as["ms"]
        .on(_.term["student_id"].get === ctx.source["s"].get.term["id"].get))
      .join(ctx => Courses.as["cc"].on(_.term["id"].get ===
        ctx.source["ms"].get.term["course_id"].get))
      .take(ctx =>
        ctx.source["s"].get.term["id"].get.as["sid"] ::
        ctx.source["s"].get.term["name"].get.as["sname"] ::
        ctx.source["ms"].get.term["max_score"].get.as["score"] ::
        //ctx.source["cc"].get.term["name"].get.as["cname"] ::
        HNil
      )
      .withPlaceholder[Int, "student_id"]
      .where(ctx => ctx.source["s"].get.term["id"].get === ctx.placeholder["student_id"].get)

  val terms: Map[String, Term[Any]] =
    TagMap(studentsSelect.terms)
}
