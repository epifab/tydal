package io.epifab.yadl

package object examples {
  case class Student(id: Int, name: String, email: Option[String])
  case class Exam(studentId: Int, courseId: Int, rate: Int)
  case class Course(id: Int, name: String)
}
