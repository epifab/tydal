package io.epifab.yadl.examples

import java.time.{Instant, LocalDate}

import io.epifab.yadl.examples.Schema.{Address, Interest}

object Model {
  case class Student(
    id: Int,
    name: String,
    email: Option[String],
    dateOfBirth: LocalDate,
    address: Option[Address],
    interests: Seq[Interest]
  )

  case class StudentExam(id: Int, name: String, score: Int, time: Instant, course: String)

  case class Exam(studentId: Int, courseId: Int, score: Int, timestamp: Instant, registration: Option[Instant])

  case class Course(id: Int, name: String)
}
