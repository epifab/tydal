package io.epifab.tydal.examples

import java.time.LocalDate
import java.util.UUID

import io.epifab.tydal._
import io.epifab.tydal.queries.{Insert, Select}
import io.epifab.tydal.runtime.{PostgresConfig, PostgresConnection}
import io.epifab.tydal.schema.{Column, FieldDecoder, FieldEncoder, TableBuilder}

case class Address(postcode: String, line1: String, line2: Option[String])

case class Student(
  id: UUID,
  name: String,
  email: Option[String],
  date_of_birth: LocalDate,
  address: Option[Address]
)

object Program extends App {
  import io.circe.generic.auto._
  implicit val addressEncoder: FieldEncoder[Address] = FieldEncoder.jsonEncoder[Address]
  implicit val addressDecoder: FieldDecoder[Address] = FieldDecoder.jsonDecoder[Address]

  object Students extends TableBuilder["students", (
    Column[UUID] As "id",
    Column[String] As "name",
    Column[Option[String]] As "email",
    Column[LocalDate] As "date_of_birth",
    Column[Option[Address]] As "address",
  )]

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
