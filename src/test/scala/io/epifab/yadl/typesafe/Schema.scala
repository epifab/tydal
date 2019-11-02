package io.epifab.yadl.typesafe

import java.time.{Instant, LocalDate}

import io.circe.{Decoder, Encoder}
import io.circe.generic.auto._
import io.epifab.yadl.typesafe.fields.{Column, FieldDecoder, FieldEncoder}
import shapeless.the

object Schema {
  abstract sealed class Interest(val value: String)

  object Interest {
    def apply(value: String): Either[String, Interest] = value match {
      case Music.value => Right(Music)
      case Art.value => Right(Art)
      case History.value => Right(History)
      case Math.value => Right(Math)
      case _ => Left("Unknown interest")
    }

    case object Music extends Interest("music")
    case object Art extends Interest("art")
    case object History extends Interest("history")
    case object Math extends Interest("math")
  }

  case class Address(postcode: String, line1: String, line2: Option[String])

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

  case class Student(
    id: Int,
    name: String,
    email: Option[String],
    dateOfBirth: LocalDate,
    address: Option[Address],
    interests: Seq[Interest]
  )

  case class Exam(studentId: Int, courseId: Int, score: Int, timestamp: Instant, registration: Option[Instant])

  case class Course(id: Int, name: String)
}
