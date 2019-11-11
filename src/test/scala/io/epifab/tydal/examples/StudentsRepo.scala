package io.epifab.tydal.examples

import java.time.LocalDate

import io.epifab.tydal.Implicits._
import io.epifab.tydal._
import io.epifab.tydal.examples.Model.{Interest, Student, StudentExam}
import io.epifab.tydal.examples.Schema._
import io.epifab.tydal.fields._
import io.epifab.tydal.runner.TransactionIO

object StudentsRepo {
  private val studentExamsQuery =
    Select
      .from(Students as "s")
      .innerJoin(Exams as "e").on(_("student_id") === _("s", "id"))
      .innerJoin(Courses as "c").on(_("id") === _("e", "course_id"))
      .take($ => (
        $("s").id            as "sid",
        $("s").name          as "sname",
        $("e").score         as "score",
        $("e").examTimestamp as "etime",
        $("c").name          as "cname"
      ))
      .where(_("s", "id") in "sids")
      .sortBy($ => Ascending($("sid")) -> Descending($("score")))
      .compile

  val studentsWithMinScore = Select
    .from(Students as "s")
    .take(_ ("s").*)
    .where($ => $("s", "id") in Select
      .from(Exams as "e")
      .take1(_("e", "student_id"))
      .where(_("e", "score") >= "min_score"))
    .compile

  def findById(id: Int): TransactionIO[Option[Student]] =
    Select
      .from(Students as "s")
      .take(_("s").*)
      .where(_("s", "id") === "student_id")
      .compile
      .withValues(Tuple1("student_id" ~~> id))
      .mapTo[Student]
      .option

  def findStudentsWithAtLeast1ExamScore(score: Int): TransactionIO[Set[Student]] = {
    studentsWithMinScore
      .withValues(Tuple1("min_score" ~~> score))
      .mapTo[Student]
      .as[Set]
  }

  def findStudentsWithBestExam: TransactionIO[Seq[StudentExam]] =
    Select
      .from(Students as "s")
      .innerJoin(
        // max score per student
        Select
          .from(Exams as "e1")
          .take($ => ($("e1").studentId as "sid1", Max($("e1").score) as "score1"))
          .groupBy1(_("e1").studentId)
          .as("me1")
      )
      .on(_("sid1") === _("s").id)
      .innerJoin(
        // select only the latest exam
        Select
          .from(Exams as "e2")
          .take($ => ($("e2").studentId as "sid2", $("e2").score as "score2", Max($("e2").examTimestamp) as "etime"))
          .groupBy($ => ($("e2").studentId, $("e2").score))
          .as("me2")
      )
      .on((me2, ctx) => me2("sid2") === ctx("me1", "sid1") and (me2("score2") === ctx("me1", "score1")))
      .innerJoin(Exams as "e")
      .on((e, ctx) => e.examTimestamp === ctx("me2", "etime") and (e.studentId === ctx("me2", "sid2")))
      .innerJoin(Courses as "c")
      .on(_.id === _("e").courseId)
      .take($ => (
        $("s").id            as "sid",
        $("s").name          as "sname",
        $("e").score         as "score",
        $("e").examTimestamp as "etime",
        $("c").name          as "cname"
      ))
      .sortBy($ => (Descending($("score")), Ascending($("sname"))))
      .compile
      .withValues(())
      .mapTo[StudentExam]
      .as[Vector]

  def findAllBy[C <: Column[_], T]
      (column: StudentsSchema => C, value: T)
      (implicit
       fieldEncoder: FieldEncoder[T],
       fieldDecoder: FieldDecoder[T],
       areComparable: AreComparable[C, NamedPlaceholder[T] with Tag["x"]]): TransactionIO[Seq[Student]] = {
    Select
      .from(Students as "s")
      .take(_("s").*)
      .where { $ => column($("s").schema) === NamedPlaceholder[T, "x"] }
      .compile
      .withValues(Tuple1("x" ~~> value))
      .mapTo[Student]
      .as[Vector]
  }

  def findAllBy(
                 minAge: Option[Int] = None,
                 maxAge: Option[Int] = None,
                 name: Option[String] = None,
                 email: Option[String] = None,
                 interests: Option[Seq[Interest]] = None
               ): TransactionIO[Seq[Student]] = {
    Select
      .from(Students as "s")
      .take(_("s").*)
      .where { $ =>
        val minAgeFilter = $("s", "date_of_birth") <= PlaceholderValueOption(minAge.map(LocalDate.now.minusYears(_)))
        val maxAgeFilter = $("s", "date_of_birth") >= PlaceholderValueOption(maxAge.map(LocalDate.now.minusYears(_)))
        val nameFilter = $("s", "name") like PlaceholderValueOption(name)
        val emailFilter = $("s", "email") like PlaceholderValueOption(email)
        val interestsFilter = $("s", "interests") overlaps PlaceholderValueOption(interests)

        minAgeFilter and maxAgeFilter and nameFilter and emailFilter and interestsFilter
      }
      .compile
      .withValues(())
      .mapTo[Student]
      .as[Vector]
  }

  def findStudentExams(ids: Seq[Int]): TransactionIO[Seq[StudentExam]] = {
    studentExamsQuery
      .withValues(Tuple1("sids" ~~> ids))
      .mapTo[StudentExam]
      .as[Vector]
  }

  def add(student: Student): TransactionIO[Int] = {
    Insert
      .into(Students)
      .compile
      .withValues {
        (
          "id" ~~> student.id,
          "name" ~~> student.name,
          "email" ~~> student.email,
          "date_of_birth" ~~> student.dateOfBirth,
          "address" ~~> student.address,
          "interests" ~~> student.interests
        )
      }
  }

  def updateNameAndEmail(id: Int, name: String, email: Option[String]): TransactionIO[Int] =
    Update(Students)
      .fields(s => (s.name, s.email))
      .where(_.id === "id")
      .compile
      .withValues {
        (
          "name" ~~> name,
          "email" ~~> email,
          "id" ~~> id
        )
      }

  def remove(id: Int): TransactionIO[Int] =
    Delete.from(Students)
      .where(_.id === "id")
      .compile
      .withValues {
        Tuple1("id" ~~> id)
      }

  lazy val removeAll: TransactionIO[Int] =
    Delete
      .from(Students)
      .compile
      .withValues(())
}
