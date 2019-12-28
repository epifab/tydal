package io.epifab.tydal.examples

import java.time.LocalDate
import java.util.UUID

import io.epifab.tydal._
import io.epifab.tydal.queries.{Insert, Select}
import io.epifab.tydal.runtime.{PostgresConfig, PostgresConnection}
import io.epifab.tydal.schema._

case class Address(postcode: String, line1: String, line2: Option[String])

case class Student(
  id: UUID,
  name: String,
  email: Option[String],
  date_of_birth: LocalDate,
  address: Option[Address]
)

object ProgramSchema {
  import io.circe.generic.auto._
  implicit val addressEncoder: FieldEncoder.Aux[Address, String] = FieldEncoder.jsonEncoder[Address]
  implicit val addressDecoder: FieldDecoder.Aux[Address, String] = FieldDecoder.jsonDecoder[Address]
}

object Program extends App {
  import ProgramSchema._

  object Students extends TableBuilder["students", (
    "id" :=: UUID,
    "name" :=: String,
    "email" :=: Option[String],
    "date_of_birth" :=: LocalDate,
    "address" :=: Option[Address]
  )]

  val connection = PostgresConnection(PostgresConfig.fromEnv())

  val createStudent =
    Insert
      .into(Students)
      .compile
      .run((
        "id" ~~> UUID.randomUUID,
        "name" ~~> "Jack",
        "email" ~~> Some("jack@tydal.io"),
        "date_of_birth" ~~> LocalDate.of(1970, 1, 1),
        "address" ~~> Some(Address("7590", "Tydalsvegen 125", Some("Tydal, Norway"))),
      ))

  val findStudents =
    Select
      .from(Students as "s")
      .focus("s").take(_.*)
      .where(ctx => ctx("s", "email") like "email" and (ctx("date_of_birth") < "max_dob"))
      .compile
      .to[Student]
      .as[Vector]
      .run((
        "email" ~~> "%@tydal.io",
        "max_dob" ~~> LocalDate.of(1986, 1, 1)
      ))

  val program = (for {
    _ <- createStudent
    students <- findStudents
  } yield students).toIO(connection)

  program.unsafeRunSync()
}
