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

  val studentsSelect: Select[Placeholder[Int, Int] with Tag["student_id"] :: HNil, Column[Int] with Tag["sid"] :: Column[String] with Tag["sname"] :: Column[Option[Int]] with Tag["score"] :: Column[String] with Tag["cname"] :: HNil, HNil, Table["students", AS[Column[Int], "id"] :: AS[Column[String], "name"] :: HNil] with Tag["s"] :: Join[SubQuery[Column[Int] with Tag["student_id"] :: Column[Option[Int]] with Tag["max_score"] :: Column[Option[Int]] with Tag["course_id"] :: HNil, Select[HNil, AS[Column[Int], "student_id"] :: Aggregation[Int, Option[Int]] with Tag["max_score"] :: Aggregation[Int, Option[Int]] with Tag["course_id"] :: HNil, AS[Column[Int], "student_id"] :: HNil, Table["exams", AS[Column[Int], "student_id"] :: AS[Column[Int], "course_id"] :: AS[Column[Int], "score"] :: HNil] with Tag["e"] :: HNil]] with Tag["ms"]] :: Join[Table["courses", AS[Column[Int], "id"] :: AS[Column[String], "name"] :: HNil] with Tag["cc"]] :: HNil] =
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
