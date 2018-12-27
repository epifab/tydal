package io.epifab.yadl.examples

import java.time.{LocalDate, LocalDateTime}

import io.epifab.yadl.domain._
import io.epifab.yadl.implicits._

sealed abstract class Interest(val value: String)

object Interest {
  def apply(value: String): Interest = value match {
    case Math.value => Math
    case History.value => History
    case Art.value => Art
    case Music.value => Music
    case _ => throw new IllegalArgumentException(s"Interest $value not found")
  }

  case object Math extends Interest("math")
  case object History extends Interest("history")
  case object Art extends Interest("art")
  case object Music extends Interest("music")
}

object Adapters {
  implicit val interestsFieldAdapter: EnumFieldAdapter[Interest] =
    new EnumFieldAdapter[Interest](
    "interest",
    _.value,
    string => scala.util.Try(Interest(string))
      .toEither
      .left.map(error => ExtractorError(error.getMessage))
  )

  implicit val addressFieldAdapter: FieldAdapter[Address] = {
    import io.circe.generic.auto._
    FieldAdapter.json[Address]
  }
}

object Schema {
  import Adapters._

  trait ExamsProjection {
    def examsCount: AggregateColumn[Int, Option[Int]]
    def avgScore: AggregateColumn[Int, Option[Double]]
    def minScore: AggregateColumn[Int, Option[Int]]
    def maxScore: AggregateColumn[Int, Option[Int]]
  }

  object ExamsProjection {
    def apply(): ExamsTable with ExamsProjection = new ExamsTable with ExamsProjection {
      override lazy val examsCount: AggregateColumn[Int, Option[Int]] = Count(courseId)
      override lazy val avgScore: AggregateColumn[Int, Option[Double]] = Avg(score)
      override lazy val minScore: AggregateColumn[Int, Option[Int]] = Min(score)
      override lazy val maxScore: AggregateColumn[Int, Option[Int]] = Max(score)
    }
  }

  class ExamsSubquery extends SubQuery {
    private val exams = ExamsProjection()

    val studentId: Column[Int] = column(exams.studentId)

    val examsCount: Column[Option[Int]] = column(exams.examsCount)
    val avgScore: Column[Option[Double]] = column(exams.avgScore)
    val minScore: Column[Option[Int]] = column(exams.minScore)
    val maxScore: Column[Option[Int]] = column(exams.maxScore)

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
    lazy val interests: TableColumn[Seq[Interest]] = column("interests")
    lazy val address: TableColumn[Option[Address]] = column("address")

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
