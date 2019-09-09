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
      .groupBy(_[E, "student_id"].get :: HNil)
      .take($ =>
        $[E, "student_id"].get ::
        Max($[E, "score"].get).as["max_score"] ::
        Min($[E, "course_id"].get).as["course_id"] ::
        HNil)
      .subQuery

  val studentsSelect =
    Select
      .from(Students.as["s"])
      .join($ => maxScoreSubQuery["e"].as["ms"]
        .on(_["student_id"].get === $["s", "id"].get))
      .join($ => Courses.as["cc"].on(_["id"].get === $["ms", "course_id"].get))
      .take($ =>
        $["s", "id"].get.as["sid"] ::
        $["s", "name"].get.as["sname"] ::
        $["ms", "max_score"].get.as["score"] ::
        $["cc", "name"].get.as["cname"] ::
        HNil
      )
      .withPlaceholder[Int, "student_id"]
      .where($ => $["s", "id"].get === $["student_id"].get)
}
