package tydal.university

import java.time.{Instant, LocalDate}
import java.util.UUID

import io.circe.generic.auto._
import tydal._
import Model._
import tydal.schema.{FieldDecoder, FieldEncoder, TableBuilder}

object Schema {
  implicit val interestDecoder: FieldDecoder.Aux[Interest, String] =
    FieldDecoder.enumDecoder("interest", Interest.apply)

  implicit val interestEncoder: FieldEncoder.Aux[Interest, String] =
    FieldEncoder.enumEncoder("interest", _.value)

  implicit val addressDecoder: FieldDecoder.Aux[Address, String] =
    FieldDecoder.jsonDecoder[Address]

  implicit val addressEncoder: FieldEncoder.Aux[Address, String] =
    FieldEncoder.jsonEncoder[Address]

  object Students extends TableBuilder["students", (
    "id" :=: UUID,
    "name" :=: String,
    "email" :=: Option[String],
    "date_of_birth" :=: LocalDate,
    "address" :=: Option[Address],
    "interests" :=: Seq[Interest],
  )]

  object Exams extends TableBuilder["exams", (
    "student_id" :=: UUID,
    "course_id" :=: UUID,
    "score" :=: Int,
    "exam_timestamp" :=: Instant,
    "registration_timestamp" :=: Option[Instant],
  )]

  object Courses extends TableBuilder["courses", (
    "id" :=: UUID,
    "name" :=: String
  )]
}
