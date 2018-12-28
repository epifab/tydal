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

  class ExamsProjection(override val table: ExamsTable) extends TableProjection[Exam, StudentExams] {
    val studentId: TableColumn[Int] = table.studentId
    val examsCount: Column[Int] = Count(table.courseId)
    val avgScore: Column[Option[Double]] = Avg(table.score)
    val minScore: Column[Option[Int]] = Min(table.score)
    val maxScore: Column[Option[Int]] = Max(table.score)

    val `*`: Selectable[StudentExams] = (
      studentId +:
        examsCount +:
        avgScore +:
        minScore +:
        maxScore +:
        SNil
    ).as[StudentExams]
  }

  class StudentsTable extends Table[Student](StudentsTable.name) {
    val id: TableColumn[Int] = column("id")
    val name: TableColumn[String] = column("name")
    val email: TableColumn[Option[String]] = column("email")
    val dateOfBirth: TableColumn[LocalDate] = column("date_of_birth")
    val address: TableColumn[Option[Address]] = column("address")
    val interests: TableColumn[Seq[Interest]] = column("interests")

    lazy val `*`: Selectable[Student] = (
      id +:
      name +:
      email +:
      dateOfBirth +:
      address +:
      interests +:
      SNil).as[Student]

    lazy val exams: Relation[ExamsTable] = (new ExamsTable).on(_.studentId === id)
  }

  object StudentsTable {
    val name = "students"
  }

  class CoursesTable extends Table[Course](CoursesTable.name) {
    val id: TableColumn[Int] = column("id")
    val name: TableColumn[String] = column("name")

    override val `*`: Selectable[Course] = (id +: name +: SNil).as[Course]

    lazy val exams: Relation[ExamsTable] = (new ExamsTable).on(_.courseId === id)
  }

  object CoursesTable {
    val name = "courses"
  }

  class ExamsTable extends Table[Exam](ExamsTable.name) {
    val studentId: TableColumn[Int] = column("student_id")
    val courseId: TableColumn[Int] = column("course_id")
    val score: TableColumn[Int] = column("score")
    val dateTime: TableColumn[LocalDateTime] = column("exam_timestamp")

    override val `*`: Selectable[Exam] = (
      studentId +:
      courseId +:
      score +:
      dateTime +:
      SNil
    ).as[Exam]

    lazy val course: Relation[CoursesTable] = (new CoursesTable).on(_.id === courseId)

    lazy val student: Relation[StudentsTable] = (new StudentsTable).on(_.id === studentId)
  }

  object ExamsTable {
    val name = "exams"
  }
}
