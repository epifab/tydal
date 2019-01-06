package io.epifab.yadl.examples

import java.time.{LocalDate, LocalDateTime}

import io.epifab.yadl.domain._
import io.epifab.yadl.implicits._
import shapeless.HNil

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
    val studentId: Column[Int] = table.studentId
    val examsCount: Field[Int] = Count(table.courseId)
    val avgScore: Field[Option[Double]] = Avg(table.score)
    val minScore: Field[Option[Int]] = Min(table.score)
    val maxScore: Field[Option[Int]] = Max(table.score)

    val `*`: Reader[StudentExams] = Reader(
      studentId ::
      examsCount ::
      avgScore ::
      minScore ::
      maxScore ::
      HNil
    )
  }

  class StudentsTable extends Table[Student](StudentsTable.name) {
    val id: Column[Int] = column("id")
    val name: Column[String] = column("name")
    val email: Column[Option[String]] = column("email")
    val dateOfBirth: Column[LocalDate] = column("date_of_birth")
    val address: Column[Option[Address]] = column("address")
    val interests: Column[Seq[Interest]] = column("interests")

    lazy val `*`: Reader[Student] = Reader(
      id ::
      name ::
      email ::
      dateOfBirth ::
      address ::
      interests ::
      HNil
    )

    lazy val exams: Relation[ExamsTable] = (new ExamsTable).on(_.studentId === id)
  }

  object StudentsTable {
    val name = "students"
  }

  class CoursesTable extends Table[Course](CoursesTable.name) {
    val id: Column[Int] = column("id")
    val name: Column[String] = column("name")

    override val `*`: Reader[Course] = Reader(id :: name :: HNil)

    lazy val exams: Relation[ExamsTable] = (new ExamsTable).on(_.courseId === id)
  }

  object CoursesTable {
    val name = "courses"
  }

  class ExamsTable extends Table[Exam](ExamsTable.name) {
    val studentId: Column[Int] = column("student_id")
    val courseId: Column[Int] = column("course_id")
    val score: Column[Int] = column("score")
    val dateTime: Column[LocalDateTime] = column("exam_timestamp")

    override val `*`: Reader[Exam] = Reader(
      studentId ::
      courseId ::
      score ::
      dateTime ::
      HNil
    )

    lazy val course: Relation[CoursesTable] = (new CoursesTable).on(_.id === courseId)

    lazy val student: Relation[StudentsTable] = (new StudentsTable).on(_.id === studentId)
  }

  object ExamsTable {
    val name = "exams"
  }
}
