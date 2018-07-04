package io.epifab.yadl

package object examples {
  case class Address(postcode: String, line1: String, line2: Option[String])

  case class Student(id: Int, name: String, email: Option[String], address: Option[Address], interests: Seq[String])
  case class Exam(studentId: Int, courseId: Int, rate: Int)
  case class Course(id: Int, name: String)
}
