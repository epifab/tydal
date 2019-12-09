package io.epifab.tydal.typesafe

import java.time.{Instant, LocalDate}

import io.epifab.tydal._
import io.epifab.tydal.examples.Schema._
import io.epifab.tydal.schema._
import io.epifab.tydal.queries.{Delete, Select, Update}
import io.epifab.tydal.runtime.Transaction

object SelectQueries {
  val queryWithRange =
    Select
      .from(Exams as "e")
      .take1(_("e", "student_id") as "sid")
      .inRange(5, 10)

  val maxScoreSubQuery =
    Select
      .from(Exams as "e")
      .groupBy1(_("e", "student_id"))
      .where(_("e", "exam_timestamp") < "min_date")
      .take($ => (
        $("e", "student_id")      as "student_id",
        Max($("e", "score"))    as "max_score",
        Min($("e", "course_id")) as "course_id"
      ))

  val studentsQuery =
    Select
      .from(Students as "s")
      .innerJoin(maxScoreSubQuery as "ms").on(_("student_id") === _("s", "id"))
      .innerJoin(Courses as "cc").on(_("id") === _("ms", "course_id"))
      .take($ => (
        $("s", "id")              as "sid",
        $("s", "name")            as "sname",
        $("ms", "max_score")      as "score",
        Nullable($("cc", "name")) as "cname"
      ))
      .where(_("s", "id") === "student_id")

  val examsWithCourseQuery =
    Select
      .from(Exams as "e")
      .innerJoin(Courses as "c").on(_("id") === _("e", "course_id"))
      .take($ => (
        $("c", "name")  as "cname",
        $("e", "score") as "score"
      ))

  val updateStudentQuery = Update(Students)
    .fields(s => (s("name"), s("email")))
    .where(_("id") === "id")

  val deleteStudentQuery = Delete.from(Students)
    .where(_("id") === "id")

  def getFields: Transaction[Seq[(Int, Seq[Double], Map[String, String], LocalDate, Instant)]] = {
    implicit val mapEnc: FieldEncoder[Map[String, String]] = FieldEncoder.jsonEncoder[Map[String, String]]
    implicit val mapDec: FieldDecoder[Map[String, String]] = FieldDecoder.jsonDecoder[Map[String, String]]

    val int = NamedPlaceholder[Int, "int"]
    val listOfDouble = NamedPlaceholder[Seq[Double], "listOfDouble"]
    val json = NamedPlaceholder[Map[String, String], "map"]
    val date = NamedPlaceholder[LocalDate, "date"]
    val instant = NamedPlaceholder[Instant, "instant"]

    Select
      .from(Students as "s")
      .take(_ => (int, listOfDouble, json, date, instant))
      .compile
      .toTuple
      .as[Vector]
      .run(
        (
          "int" ~~> 1,
          "listOfDouble" ~~> Seq(3.0, 9.99),
          "map" ~~> Map("blue" -> "sky", "yellow" -> "banana"),
          "date" ~~> LocalDate.of(1992, 2, 25),
          "instant" ~~> Instant.parse("1986-03-08T09:00:00z"),
        )
      )
  }
}
