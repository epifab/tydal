package io.epifab.yadl.examples

import java.time.{LocalDate, LocalDateTime}

import io.epifab.yadl.domain._

object Schema {
  import io.circe.generic.auto._
  import io.epifab.yadl.implicits._

  trait ExamsProjection {
    def examsCount: Column[Option[Int]]
    def avgScore: Column[Option[Double]]
    def minScore: Column[Option[Int]]
    def maxScore: Column[Option[Int]]
  }

  object ExamsProjection {
    def apply(): ExamsTable with ExamsProjection = new ExamsTable with ExamsProjection {
      override lazy val examsCount: AggregateColumn[Int, Option[Int]] = Count(courseId)
      override lazy val avgScore: AggregateColumn[Int, Option[Double]] = Avg(score)
      override lazy val minScore: AggregateColumn[Int, Option[Int]] = Min(score)
      override lazy val maxScore: AggregateColumn[Int, Option[Int]] = Max(score)
    }
  }

  class ExamsSubquery extends SubQuery with ExamsProjection {
    private val exams = ExamsProjection()

    val studentId: Column[Int] = column(exams.studentId)

    override val examsCount: Column[Option[Int]] = column(exams.examsCount)
    override val avgScore: Column[Option[Double]] = column(exams.avgScore)
    override val minScore: Column[Option[Int]] = column(exams.minScore)
    override val maxScore: Column[Option[Int]] = column(exams.maxScore)

    def select: Select =
      Select
        .from(exams)
        .take(exams.studentId)
        .aggregateBy(
          exams.examsCount,
          exams.avgScore,
          exams.minScore,
          exams.maxScore
        )
  }

  class StudentsTable extends Table(StudentsTable.name) {
    lazy val id: TableColumn[Int] = column("id")
    lazy val name: TableColumn[String] = column("name")
    lazy val email: TableColumn[Option[String]] = column("email")
    lazy val dateOfBirth: TableColumn[LocalDate] = column("date_of_birth")
    lazy val interests: TableColumn[Seq[String]] = column("interests")
    lazy val address: TableColumn[Option[Json[Address]]] = column("address")

    lazy val `*`: Seq[TableColumn[_]] = Seq(id, name, email, dateOfBirth, interests, address)

    lazy val exams: Relation[ExamsTable] = (new ExamsTable).on(_.studentId === id)
  }

  object StudentsTable {
    val name = "students"
  }

  class CoursesTable extends Table(CoursesTable.name) {
    lazy val id: TableColumn[Int] = column("id")
    lazy val name: TableColumn[String] = column("name")

    lazy val `*`: Seq[TableColumn[_]] = Seq(id, name)

    lazy val exams: Relation[ExamsTable] = (new ExamsTable).on(_.courseId === id)
  }

  object CoursesTable {
    val name = "courses"
  }

  class ExamsTable extends Table(ExamsTable.name) {
    lazy val studentId: TableColumn[Int] = column("student_id")
    lazy val courseId: TableColumn[Int] = column("course_id")
    lazy val score: TableColumn[Int] = column("score")
    lazy val dateTime: TableColumn[LocalDateTime] = column("exam_timestamp")

    lazy val `*`: Seq[Column[_]] = Seq(studentId, courseId, score, dateTime)

    lazy val course: Relation[CoursesTable] = (new CoursesTable).on(_.id === courseId)

    lazy val student: Relation[StudentsTable] = (new StudentsTable).on(_.id === studentId)
  }

  object ExamsTable {
    val name = "exams"
  }
}
