package io.epifab.yadl

import java.time.{LocalDate, LocalDateTime}

package object examples {
  case class Address(postcode: String, line1: String, line2: Option[String])

  case class Student(
    id: Int,
    name: String,
    email: Option[String],
    dateOfBirth: LocalDate,
    address: Option[Address],
    interests: Seq[String]
  )
  case class StudentExams(
    studentId: Int,
    count: Option[Int],
    avgScore: Option[Double],
    minScore: Option[Int],
    maxScore: Option[Int]
  )
  case class Exam(studentId: Int, courseId: Int, score: Int, dateTime: LocalDateTime)
  case class Course(id: Int, name: String)
}
