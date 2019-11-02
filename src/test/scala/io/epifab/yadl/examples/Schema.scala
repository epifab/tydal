package io.epifab.yadl.examples

import java.time.{Instant, LocalDate}

import io.circe.generic.auto._
import io.circe.{Decoder, Encoder}
import io.epifab.yadl._
import io.epifab.yadl.examples.Model.{Address, Interest}
import io.epifab.yadl.fields.{Column, FieldDecoder, FieldEncoder}
import shapeless.the

object Schema {
  implicit val interestDecoder: FieldDecoder.Aux[Interest, String] =
    FieldDecoder.enumDecoder("interest", Interest.apply)

  implicit val interestEncoder: FieldEncoder.Aux[Interest, String] =
    FieldEncoder.enumEncoder("interest", _.value)

  implicit val addressDecoder: FieldDecoder.Aux[Address, String] =
    FieldDecoder.jsonDecoder(the[Decoder[Address]])

  implicit val addressEncoder: FieldEncoder.Aux[Address, String] =
    FieldEncoder.jsonEncoder(the[Encoder[Address]])

  case class StudentsSchema(
    id: Column[Int] AS "id",
    name: Column[String] AS "name",
    email: Column[Option[String]] AS "email",
    dateOfBirth: Column[LocalDate] AS "date_of_birth",
    address: Column[Option[Address]] AS "address",
    interests: Column[Seq[Interest]] AS "interests"
  )

  case class ExamsSchema(
    studentId: Column[Int] AS "student_id",
    courseId: Column[Int] AS "course_id",
    score: Column[Int] AS "score",
    examTimestamp: Column[Instant] AS "exam_timestamp",
    registrationTimestamp: Column[Option[Instant]] AS "registration_timestamp"
  )

  case class CoursesSchema(id: Column[Int] AS "id", name: Column[String] AS "name")

  object Students extends TableBuilder["students", StudentsSchema]

  object Exams extends TableBuilder["exams", ExamsSchema]

  object Courses extends TableBuilder["courses", CoursesSchema]
}
