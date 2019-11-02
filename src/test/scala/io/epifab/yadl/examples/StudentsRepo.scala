package io.epifab.yadl.examples

import io.epifab.yadl.examples.Model.{Student, StudentExam}
import io.epifab.yadl.examples.Schema.{Courses, Exams, Students}
import io.epifab.yadl.runner.TransactionIO
import io.epifab.yadl.{Delete, Insert, Select, Update}
import io.epifab.yadl.Implicits._

object StudentsRepo {
  private val studentExamsQuery = Select.from(Students as "s")
    .join($ => (Exams as "e").on(_ ("student_id") === $("s", "id")))
    .join($ => (Courses as "c").on(_ ("id") === $("e", "course_id")))
    .take($ => (
      $("s", "id").as["sid"],
      $("s", "name").as["sname"],
      $("e", "score").as["rate"],
      $("e", "exam_timestamp").as["etime"],
      $("c", "name").as["cname"]
    ))
    .where(_ ("s", "id") === "sid")
    .compile

  def findById(id: Int): TransactionIO[Option[Student]] =
    Select
      .from(Students as "s")
      .take(_("s").*)
      .where(_("s", "id") === "student_id")
      .compile
      .withValues(Tuple1("student_id" ~~> id))
      .takeFirst
      .mapTo[Student]

  def findStudentExams(id: Int): TransactionIO[Seq[StudentExam]] = {
    studentExamsQuery
      .withValues(Tuple1("sid" ~~> id))
      .mapTo[StudentExam]
  }

  def add(student: Student): TransactionIO[Int] = {
    Insert
      .into(Students)
      .compile
      .withValues {
        (
          "id" ~~> student.id,
          "name" ~~> student.name,
          "email" ~~> student.email,
          "date_of_birth" ~~> student.dateOfBirth,
          "address" ~~> student.address,
          "interests" ~~> student.interests
        )
      }
  }

  def updateNameAndEmail(id: Int, name: String, email: Option[String]): TransactionIO[Int] =
    Update(Students)
      .fields(s => (s.name, s.email))
      .where(_.id === "id")
      .compile
      .withValues {
        (
          "name" ~~> name,
          "email" ~~> email,
          "id" ~~> id
        )
      }

  def remove(id: Int): TransactionIO[Int] =
    Delete.from(Students)
      .where(_.id === "id")
      .compile
      .withValues {
        Tuple1("id" ~~> id)
      }

  lazy val removeAll: TransactionIO[Int] =
    Delete
      .from(Students)
      .compile
      .withValues(())
}
