package io.epifab.yadl.typesafe

import java.time.Instant

import io.epifab.yadl.typesafe.Implicits._
import io.epifab.yadl.typesafe.Schema._
import io.epifab.yadl.typesafe.fields._

object SelectQueries {
  val studentExams =
    Select.from(Students as "s")
      .join($ => (Exams as "e").on(_("student_id") === $("s", "id")))
      .join($ => (Courses as "c").on(_("id") === $("e", "course_id")))
      .take($ => (
        $("s", "id").as["sid"],
        $("s", "name").as["sname"],
        $("e", "score").as["rate"],
        $("e", "exam_timestamp").as["etime"],
        $("c", "name").as["cname"]
      ))
      .where(_("s", "id") === Placeholder[Int, "sid"])
      .compile

  val studentsQuery = {
    val maxScoreSubQuery =
      Select
        .from(Exams as "e")
        .groupBy1(_("e", "student_id"))
        .where(_("e", "registration_timestamp") < Placeholder[Instant, "min_date"])
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
      .where(_("s", "id") === Placeholder[Int, "student_id"])
  }

  val examsWithCourseQuery =
    Select
      .from(Exams as "e")
      .join($ => (Courses as "c").on(_("id") === $("e", "course_id")))
      .take($ => (
        $("c", "name").as["cname"],
        $("e", "score").as["score"]
      ))
}
