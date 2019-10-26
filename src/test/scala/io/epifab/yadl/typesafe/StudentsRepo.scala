package io.epifab.yadl.typesafe

import java.sql.Connection
import java.time.LocalDate

import cats.effect.IO
import io.epifab.yadl.typesafe.Implicits._
import io.epifab.yadl.typesafe.Schema._
import io.epifab.yadl.typesafe.fields.{Placeholder, Value}

class StudentsRepo {
  case class Student(
    id: Int,
    name: String,
    email: Option[String],
    dateOfBirth: LocalDate,
    address: Option[Address],
    interests: Seq[Interest]
  )

  private val insert =
    Insert.into(Students)
      .set((
        Value("id", 3),
        Value("name", "John"),
        Value("email", Some("john@gmail.com")),
        Value("date_of_birth", LocalDate.of(1096, 3, 8)),
        Value("address", Some(Address("N1 000", "123 Fake St.", Some("Finsbury Park")))),
        Value("interests", Seq(Math, Art))
      ))

  private val studentByIdQuery =
    Select
      .from(Students as "s")
      .take(_ ("s").*)
      .where(_("s", "id") === Placeholder["student_id", Int])
      .compile

  def findStudentById(connection: Connection, id: Int): IO[Either[DataError, Option[Student]]] = {
    studentByIdQuery
      .run(connection) {
        Tuple1(Value("student_id", id))
      }
      .takeFirst
      .mapTo[Student]
  }
}
