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
