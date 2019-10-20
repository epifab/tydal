package io.epifab.yadl.examples

import java.time.{Instant, LocalDate}

import io.epifab.yadl.typesafe._
import io.epifab.yadl.typesafe.fields._

object TypesafeSchema {
  abstract sealed class Interest(val value: String)
  case object Music extends Interest("music")
  case object Art extends Interest("art")
  case object History extends Interest("history")
  case object Math extends Interest("math")

  object Interest {
    def apply(value: String): Either[String, Interest] = value match {
      case Music.value => Right(Music)
      case Art.value => Right(Art)
      case History.value => Right(History)
      case Math.value => Right(Math)
      case _ => Left("Unknown interest")
    }
  }

  object Codecs {
    implicit val interestDecoder: FieldDecoder.Aux[Interest, String] =
      FieldDecoder.enumDecoder("interest", Interest.apply)

    implicit val interestEncoder: FieldEncoder.Aux[Interest, String] =
      FieldEncoder.enumEncoder("interest", _.value)
  }

  case class StudentsSchema(
    id: Column[Int] AS "id",
    name: Column[String] AS "name",
    email: Column[String] AS "email",
    dateOfBirth: Column[LocalDate] AS "date_of_birth",
    interests: Column[Seq[Interest]] AS "interests"
  )

  case class ExamsSchema(
    studentId: Column[Int] AS "student_id",
    courseIdd: Column[Int] AS "course_id",
    score: Column[Int] AS "score",
    examTimestamp: Column[Instant] AS "exam_timestamp",
    registrationTimestamp: Column[Instant] AS "registration_timestamp"
  )

  case class CoursesSchema(id: Column[Int] AS "id", name: Column[String] AS "name")

  object Students extends TableBuilder["students", StudentsSchema]

  object Exams extends TableBuilder["exams", ExamsSchema]

  object Courses extends TableBuilder["courses", CoursesSchema]
}
