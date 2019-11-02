package io.epifab.yadl.typesafe

import java.time.{Instant, LocalDate}

import io.circe.Decoder
import io.epifab.yadl.runner.TransactionIO
import io.epifab.yadl.Implicits._
import io.epifab.yadl.{Delete, Select, Update}
import io.epifab.yadl.examples.Schema._
import io.epifab.yadl.fields._
import shapeless.the

object SelectQueries {
  val studentsQuery = {
    val maxScoreSubQuery =
      Select
        .from(Exams as "e")
        .groupBy1(_("e", "student_id"))
        .where(_("e", "registration_timestamp") < "min_date")
        .take($ => (
          $("e", "student_id"),
          Max($("e", "score")).as["max_score"],
          Min($("e", "course_id")).as["course_id"]
        ))
        .subQuery

    Select
      .from(Students as "s")
      .join($ => maxScoreSubQuery.as["ms"]
        .on(_("student_id") === $("s", "id")))
      .join($ => (Courses as "cc").on(_("id") === $("ms", "course_id")))
      .take($ => (
        $("s", "id").as["sid"],
        $("s", "name").as["sname"],
        $("ms", "max_score").as["score"],
        Nullable($("cc", "name")).as["cname"]
      ))
      .where(_("s", "id") === "student_id")
  }

  val examsWithCourseQuery =
    Select
      .from(Exams as "e")
      .join($ => (Courses as "c").on(_("id") === $("e", "course_id")))
      .take($ => (
        $("c", "name").as["cname"],
        $("e", "score").as["score"]
      ))

  val updateStudentQuery = Update(Students)
    .fields(s => (s.name, s.email))
    .where(_.id === "id")

  val deleteStudentQuery = Delete.from(Students)
    .where(_.id === "id")

  def getFields: TransactionIO[Seq[(Int, Seq[Double], Map[String, String], LocalDate, Instant)]] = {
    implicit val mapEnc: FieldEncoder[Map[String, String]] = FieldEncoder.jsonEncoder
    implicit val mapDec: FieldDecoder[Map[String, String]] = FieldDecoder.jsonDecoder(the[Decoder[Map[String, String]]])

    val int = Placeholder[Int, "int"]
    val listOfDouble = Placeholder[Seq[Double], "listOfDouble"]
    val json = Placeholder[Map[String, String], "map"]
    val date = Placeholder[LocalDate, "date"]
    val instant = Placeholder[Instant, "instant"]

    Select
      .from(Students as "s")
      .take(_ => (int, listOfDouble, json, date, instant))
      .compile
      .withValues(
        (
          "int" ~~> 1,
          "listOfDouble" ~~> Seq(3.0, 9.99),
          "map" ~~> Map("blue" -> "sky", "yellow" -> "banana"),
          "date" ~~> LocalDate.of(1992, 2, 25),
          "instant" ~~> Instant.parse("1986-03-08T09:00:00z"),
        )
      )
      .tuple
  }
}
