package io.epifab.tydal.examples

import java.time.{Instant, LocalDate}

import io.circe.generic.auto._
import io.epifab.tydal._
import io.epifab.tydal.examples.Model._
import io.epifab.tydal.schema.{FieldDecoder, FieldEncoder, TableBuilder}

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
    "id" :=: Int,
    "name" :=: String,
    "email" :=: Option[String],
    "date_of_birth" :=: LocalDate,
    "address" :=: Option[Address],
    "interests" :=: Seq[Interest],
  )]

  object Exams extends TableBuilder["exams", (
    "student_id" :=: Int,
    "course_id" :=: Int,
    "score" :=: Int,
    "exam_timestamp" :=: Instant,
    "registration_timestamp" :=: Option[Instant],
  )]

  object Courses extends TableBuilder["courses", (
    "id" :=: Int,
    "name" :=: String
  )]
}
