package io.epifab.tydal.examples

import io.circe.generic.auto._
import io.epifab.tydal._
import io.epifab.tydal.examples.Model.{Address, Course, Exam, Interest, Student}
import io.epifab.tydal.fields.{FieldDecoder, FieldEncoder}

object Schema {
  implicit val interestDecoder: FieldDecoder.Aux[Interest, String] =
    FieldDecoder.enumDecoder("interest", Interest.apply)

  implicit val interestEncoder: FieldEncoder.Aux[Interest, String] =
    FieldEncoder.enumEncoder("interest", _.value)

  implicit val addressDecoder: FieldDecoder.Aux[Address, String] =
    FieldDecoder.jsonDecoder[Address]

  implicit val addressEncoder: FieldEncoder.Aux[Address, String] =
    FieldEncoder.jsonEncoder[Address]

  object Students extends TableBuilder["students", Student]

  object Exams extends TableBuilder["exams", Exam]

  object Courses extends TableBuilder["courses", Course]
}
