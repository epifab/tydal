package io.epifab.yadl

import java.time.LocalDateTime

package object examples {
  case class Address(postcode: String, line1: String, line2: Option[String])

  case class Student(id: Int, name: String, email: Option[String], address: Option[Address], interests: Seq[String])
  case class Exam(studentId: Int, courseId: Int, rate: Int, dateTime: LocalDateTime)
  case class Course(id: Int, name: String)
}
