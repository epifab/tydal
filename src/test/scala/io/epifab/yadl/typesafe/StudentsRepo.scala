package io.epifab.yadl.typesafe

import java.sql.Connection

import cats.effect.IO
import io.epifab.yadl.typesafe.Implicits._
import io.epifab.yadl.typesafe.Schema._
import io.epifab.yadl.typesafe.fields.{Placeholder, Value}

object StudentsRepo {
  def findStudentById(connection: Connection, id: Int): IO[Either[DataError, Option[Student]]] = {
    Select
      .from(Students as "s")
      .take(_ ("s").*)
      .where(_ ("s", "id") === Placeholder[Int, "student_id"])
      .compile
      .run(connection) {
        Tuple1(Value("student_id", id))
      }
      .takeFirst
      .mapTo[Student]
  }

  def insert(connection: Connection, student: Student): IOEither[DataError, Int] = {
    Insert.into(Students)
      .compile
      .run(connection) {
        (
          Value("id", student.id),
          Value("name", student.name),
          Value("email", student.email),
          Value("date_of_birth", student.dateOfBirth),
          Value("address", student.address),
          Value("interests", student.interests)
        )
      }
  }

  def updateNameAndEmail(connection: Connection, id: Int, name: String, email: Option[String]): IOEither[DataError, Int] =
    Update(Students)
      .fields(s => (s.name, s.email))
      .where(_.id === Placeholder[Int, "id"])
      .compile
      .run(connection) {
        (
          Value("name", name),
          Value("email", email),
          Value("id", id)
        )
      }
}
