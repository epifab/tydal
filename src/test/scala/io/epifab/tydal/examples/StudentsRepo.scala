package io.epifab.tydal.examples

import java.time.{Instant, LocalDate}

import io.epifab.tydal._
import io.epifab.tydal.examples.Model.{Interest, Student, StudentExam}
import io.epifab.tydal.examples.Schema._
import io.epifab.tydal.queries._
import io.epifab.tydal.runtime.{ReadStatement, Transaction}
import io.epifab.tydal.schema._

object StudentsRepo {
  private val studentExamsQuery =
    Select
      .from(Students as "s")
      .innerJoin(Exams as "e").on(_("student_id") === _("s", "id"))
      .innerJoin(Courses as "c").on(_("id") === _("e", "course_id"))
      .focus("s", "e", "c").take { case (s, e, c) => (
        s("id")             as "sid",
        s("name")           as "sname",
        e("score")          as "score",
        e("exam_timestamp") as "etime",
        c("name")           as "cname"
      )}
      .where(_("s", "id") in "sids")
      .sortBy($ => Ascending($("sid")) -> Descending($("score")))
      .compile

  val studentsWithMinScore: ReadStatement[Tuple1[Literal[Int] with Tagging["min_score"]], Student, Set] =
    Select
      .from(Students as "s")
      .take(_ ("s").*)
      .where(_("s", "id") in Select
        .from(Exams as "e")
        .take1(_("e", "student_id"))
        .where(_("e", "score") >= "min_score")
      )
      .compile
      .to[Student]
      .as[Set]

  def findById(id: Int): Transaction[Option[Student]] =
    Select
      .from(Students as "s")
      .take(_("s").*)
      .where(_("s", "id") === "student_id")
      .compile
      .to[Student]
      .asOption
      .run(Tuple1("student_id" ~~> id))

  def findStudentsWithAtLeast1ExamScore(score: Int): Transaction[Set[Student]] =
    studentsWithMinScore
      .run(Tuple1("min_score" ~~> score))

  val findStudentsWithAtLeast2Exams: Transaction[Set[(Int, Option[Double])]] =
    Select
      .from(Students as "s")
      .innerJoin(Exams as "e").on(_("student_id") === _("s", "id"))
      .focus("s", "e").take { case (s, e) => (
        s("id") as "sid",
        Avg(e("score")) as "score_avg"
      )}
      .groupBy1(_("s", "id"))
      .having(ctx => Count(ctx("e", "score")) >= "min_num_exams")
      .compile
      .toTuple
      .as[Set]
      .run(Tuple1("min_num_exams" ~~> 2))

  val findStudentsWithBestExam: Transaction[Seq[StudentExam]] =
    Select
      .from(Students as "s")
      .innerJoin(
        // max score per student
        Select
          .from(Exams as "e1")
          .focus("e1").take(e1 => (
            e1("student_id") as "sid",
            Max(e1("score")) as "score"
          ))
          .where(_("e1", "exam_timestamp") > "exam_min_date")
          .groupBy1(_("e1", "student_id"))
          .as("me1")
      )
      .on(_("sid") === _("s", "id"))
      .innerJoin(
        // select only the latest exam
        Select
          .from(Exams as "e2")
          .focus("e2").take(e2 => (
            e2("student_id") as "sid",
            e2("score") as "score",
            Max(e2("exam_timestamp")) as "etime"
          ))
          .focus("e2").groupBy(e2 => (e2("student_id"), e2("score")))
          .as("me2")
      )
      .on((me2, ctx) => me2("sid") === ctx("me1", "sid") and (me2("score") === ctx("me1", "score")))
      .innerJoin(Exams as "e")
      .on((e, ctx) => e("exam_timestamp") === ctx("me2", "etime") and (e("student_id") === ctx("me2", "sid")))
      .innerJoin(Courses as "c")
      .on(_("id") === _("e", "course_id"))
      .focus("s", "e", "c").take { case (s, e, c) => (
        s("id")             as "sid",
        s("name")           as "sname",
        e("score")          as "score",
        e("exam_timestamp") as "etime",
        c("name")           as "cname"
      )}
      .focus("s").where(s => s("date_of_birth") > "student_min_dob" and (s("date_of_birth") < "student_max_dob"))
      .sortBy($ => Descending($("score")) -> Ascending($("sname")))
      .compile
      .to[StudentExam]
      .as[Vector]
      .run((
        "exam_min_date" ~~> Instant.parse("2010-01-01T00:00:00Z"),
        "student_min_dob" ~~> LocalDate.of(1994, 1, 1),
        "student_max_dob" ~~> LocalDate.of(1998, 12, 31)
      ))

  def findAllBy(
    minAge: Option[Int] = None,
    maxAge: Option[Int] = None,
    name: Option[String] = None,
    email: Option[String] = None,
    interests: Option[Seq[Interest]] = None
  ): Transaction[Seq[Student]] = {
    Select
      .from(Students as "s")
      .take(_("s").*)
      .focus("s").where { s =>
        val minAgeFilter = minAge.map(years => s("date_of_birth") <= Literal(LocalDate.now.minusYears(years))).toFilter
        val maxAgeFilter = maxAge.map(years => s("date_of_birth") >= Literal(LocalDate.now.minusYears(years))).toFilter
        val nameFilter = name.map(s("name") like Literal(_)).toFilter
        val emailFilter = email.map(s("email") like Literal(_)).toFilter
        val interestsFilter = interests.map(s("interests") overlaps Literal(_)).toFilter

        minAgeFilter and maxAgeFilter and nameFilter and emailFilter and interestsFilter
      }
      .compile
      .to[Student]
      .as[Vector]
      .run(())
  }

  def findStudentExams(ids: Seq[Int]): Transaction[Seq[StudentExam]] = {
    studentExamsQuery
      .to[StudentExam]
      .as[Vector]
      .run(Tuple1("sids" ~~> ids))
  }

  def add(student: Student): Transaction[Int] = {
    Insert
      .into(Students)
      .compile
      .run {
        (
          "id" ~~> student.id,
          "name" ~~> student.name,
          "email" ~~> student.email,
          "date_of_birth" ~~> student.date_of_birth,
          "address" ~~> student.address,
          "interests" ~~> student.interests
        )
      }
  }

  def updateNameAndEmail(id: Int, name: String, email: Option[String]): Transaction[Int] =
    Update(Students)
      .fields(s => s("name") -> s("email"))
      .where(_("id") === "id")
      .compile
      .run {
        (
          "name" ~~> name,
          "email" ~~> email,
          "id" ~~> id
        )
      }

  def remove(id: Int): Transaction[Int] =
    Delete.from(Students)
      .where(_("id") === "id")
      .compile
      .run {
        Tuple1("id" ~~> id)
      }

  lazy val removeAll: Transaction[Int] =
    Delete
      .from(Students)
      .compile
      .run(())
}
