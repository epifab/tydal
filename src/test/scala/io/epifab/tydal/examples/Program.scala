package io.epifab.tydal.examples

import java.time.LocalDate
import java.util.UUID

import io.epifab.tydal._
import io.epifab.tydal.fields.{FieldDecoder, FieldEncoder}

case class Address(postcode: String, line1: String, line2: Option[String])

case class Student(
  id: UUID,
  name: String,
  email: Option[String],
  date_of_birth: LocalDate,
  address: Option[Address]
)

object Students extends TableBuilder["students", Student]

object Programme extends App {
  import io.circe.generic.auto._
  implicit val addressEncoder: FieldEncoder[Address] = FieldEncoder.jsonEncoder[Address]
  implicit val addressDecoder: FieldDecoder[Address] = FieldDecoder.jsonDecoder[Address]

  val connection = PostgresConnection(PostgresConfig.fromEnv())

  val createStudent =
    Insert
      .into(Students)
      .compile
      .withValues(Student(
        UUID.randomUUID,
        "Jack",
        Some("jack@tydal.io"),
        LocalDate.of(1970, 1, 1),
        Some(Address("7590", "Tydalsvegen 125", Some("Tydal, Norway"))),
      ))

  val findStudents =
    Select
      .from(Students as "s")
      .take(_("s").*)
      .where(ctx => ctx("s", "email") like "email" and (ctx("date_of_birth") < "max_dob"))
      .compile
      .withValues((
        "email" ~~> "%@tydal.io",
        "max_dob" ~~> LocalDate.of(1986, 1, 1)
      ))
      .mapTo[Student]
      .as[Vector]

  val program = (for {
    _ <- createStudent
    students <- findStudents
  } yield students).toIO(connection)

  program.unsafeRunSync()
}
