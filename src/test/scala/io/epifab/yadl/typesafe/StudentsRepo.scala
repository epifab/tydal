package io.epifab.yadl.typesafe

import java.sql.Connection

import cats.effect.IO
import io.epifab.yadl.typesafe.Implicits._
import io.epifab.yadl.typesafe.Schema._

object StudentsRepo {
  def findStudentById(connection: Connection, id: Int): IO[Either[DataError, Option[Student]]] = {
    Select
      .from(Students as "s")
      .take(_("s").*)
      .where(_("s", "id") === "student_id")
      .compile
      .run(connection) {
        Tuple1("student_id" ~> id)
      }
      .takeFirst
      .mapTo[Student]
  }

  def insert(connection: Connection, student: Student): IOEither[DataError, Int] = {
    Insert.into(Students)
      .compile
      .run(connection) {
        (
          "id" ~> student.id,
          "name" ~> student.name,
          "email" ~> student.email,
          "date_of_birth" ~> student.dateOfBirth,
          "address" ~> student.address,
          "interests" ~> student.interests
        )
      }
  }

  def updateNameAndEmail(connection: Connection, id: Int, name: String, email: Option[String]): IOEither[DataError, Int] =
    Update(Students)
      .fields(s => (s.name, s.email))
      .where(_.id === "id")
      .compile
      .run(connection) {
        (
          "name" ~> name,
          "email" ~> email,
          "id" ~> id
        )
      }

  def deleteStudent(connection: Connection, id: Int): IOEither[DataError, Int] =
    Delete.from(Students)
      .where(_.id === "id")
      .compile
      .run(connection) {
        Tuple1("id" ~> id)
      }
}
