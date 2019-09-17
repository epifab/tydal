package io.epifab.yadl.examples

import java.time.{Instant, LocalDate}

import io.epifab.yadl.typesafe.Implicits._
import io.epifab.yadl.typesafe._
import io.epifab.yadl.typesafe.fields._
import shapeless.{::, HNil}

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

  implicit val interestDecoder: FieldDecoder.Aux[Interest, String] =
    FieldDecoder.enumDecoder("interest", Interest.apply)

  implicit val interestEncoder: FieldEncoder.Aux[Interest, String] =
    FieldEncoder.enumEncoder("interest", _.value)

  object Students extends TableBuilder[
    "students",
      (Column[Int] AS "id") ::
      (Column[String] AS "name") ::
      (Column[String] AS "email") ::
      (Column[LocalDate] AS "date_of_birth") ::
      (Column[Seq[Interest]] AS "interests") ::
      HNil
  ]

  object Exams extends TableBuilder[
    "exams",
      (Column[Int] AS "student_id") ::
      (Column[Int] AS "course_id") ::
      (Column[Int] AS "score") ::
      (Column[Instant] AS "exam_timestamp") ::
      (Column[Instant] AS "registration_timestamp") ::
      HNil
  ]

  object Courses extends TableBuilder[
    "courses",
      (Column[Int] AS "id") ::
      (Column[String] AS "name") ::
      HNil
  ]
}
