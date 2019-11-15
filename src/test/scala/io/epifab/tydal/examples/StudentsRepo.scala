package io.epifab.tydal.examples

import java.time.{Instant, LocalDate}

import io.epifab.tydal.Implicits._
import io.epifab.tydal._
import io.epifab.tydal.examples.Model.{Interest, Student, StudentExam}
import io.epifab.tydal.examples.Schema._
import io.epifab.tydal.fields._
import io.epifab.tydal.runner.Transaction

object StudentsRepo {
  private val studentExamsQuery =
    Select
      .from(Students as "s")
      .innerJoin(Exams as "e").on(_("student_id") === _("s", "id"))
      .innerJoin(Courses as "c").on(_("id") === _("e", "course_id"))
      .take($ => (
        $("s", "id")             as "sid",
        $("s", "name")           as "sname",
        $("e", "score")          as "score",
        $("e", "exam_timestamp") as "etime",
        $("c", "name")           as "cname"
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

  def findById(id: Int): Transaction[Option[Student]] =
    Select
      .from(Students as "s")
      .take(_("s").*)
      .where(_("s", "id") === "student_id")
      .compile
      .withValues(Tuple1("student_id" ~~> id))
      .mapTo[Student]
      .option

  def findStudentsWithAtLeast1ExamScore(score: Int): Transaction[Set[Student]] = {
    studentsWithMinScore
      .withValues(Tuple1("min_score" ~~> score))
      .mapTo[Student]
      .as[Set]
  }

  val findStudentsWithAtLeast2Exams: Transaction[Set[(Int, Option[Double])]] =
    Select
      .from(Students as "s")
      .innerJoin(Exams as "e").on(_("student_id") === _("s", "id"))
      .take(ctx => (
        ctx("s", "id") as "sid",
        Avg(ctx("e", "score")) as "score_avg"
      ))
      .groupBy1(_("s", "id"))
      .having(ctx => Count(ctx("e", "score")) >= "min_num_exams")
      .compile
      .withValues(Tuple1("min_num_exams" ~~> 2))
      .tuple
      .as[Set]

  val findStudentsWithBestExam: Transaction[Seq[StudentExam]] =
    Select
      .from(Students as "s")
      .innerJoin(
        // max score per student
        Select
          .from(Exams as "e1")
          .take($ => (
            $("e1", "student_id") as "sid",
            Max($("e1", "score")) as "score"
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
          .take($ => (
            $("e2", "student_id") as "sid",
            $("e2", "score") as "score",
            Max($("e2", "exam_timestamp")) as "etime"
          ))
          .groupBy($ => ($("e2", "student_id"), $("e2", "score")))
          .as("me2")
      )
      .on((me2, ctx) => me2("sid") === ctx("me1", "sid") and (me2("score") === ctx("me1", "score")))
      .innerJoin(Exams as "e")
      .on((e, ctx) => e("exam_timestamp") === ctx("me2", "etime") and (e("student_id") === ctx("me2", "sid")))
      .innerJoin(Courses as "c")
      .on(_("id") === _("e", "course_id"))
      .take($ => (
        $("s", "id")             as "sid",
        $("s", "name")           as "sname",
        $("e", "score")          as "score",
        $("e", "exam_timestamp") as "etime",
        $("c", "name")           as "cname"
      ))
      .where(ctx => ctx("s", "date_of_birth") > "student_min_dob" and (ctx("s", "date_of_birth") < "student_max_dob"))
      .sortBy($ => Descending($("score")) -> Ascending($("sname")))
      .compile
      .withValues((
        "exam_min_date" ~~> Instant.parse("2010-01-01T00:00:00Z"),
        "student_min_dob" ~~> LocalDate.of(1994, 1, 1),
        "student_max_dob" ~~> LocalDate.of(1998, 12, 31)
      ))
      .mapTo[StudentExam]
      .as[Vector]

  def findAllBy[C <: Column[_], T]
      (column: StudentsSchema => C, value: T)
      (implicit
       fieldEncoder: FieldEncoder[T],
       fieldDecoder: FieldDecoder[T],
       areComparable: AreComparable[C, NamedPlaceholder[T] with Tagging["x"]]): Transaction[Seq[Student]] = {
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
               ): Transaction[Seq[Student]] = {
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

  def findStudentExams(ids: Seq[Int]): Transaction[Seq[StudentExam]] = {
    studentExamsQuery
      .withValues(Tuple1("sids" ~~> ids))
      .mapTo[StudentExam]
      .as[Vector]
  }

  def add(student: Student): Transaction[Int] = {
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

  def updateNameAndEmail(id: Int, name: String, email: Option[String]): Transaction[Int] =
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

  def remove(id: Int): Transaction[Int] =
    Delete.from(Students)
      .where(_.id === "id")
      .compile
      .withValues {
        Tuple1("id" ~~> id)
      }

  lazy val removeAll: Transaction[Int] =
    Delete
      .from(Students)
      .compile
      .withValues(())
}
