package io.epifab.yadl.examples

import cats.Applicative
import cats.implicits._
import io.epifab.yadl.domain._
import io.epifab.yadl.implicits._

import scala.language.higherKinds

class StudentsRepo[F[_]](implicit queryRunner: QueryRunner[F], a: Applicative[F]) {
  private lazy val students = new Schema.StudentsTable("s")

  import io.epifab.yadl.implicits._

  implicit private val studentExtractor: Extractor[Student] = row => for {
    id <- row.get(students.id)
    name <- row.get(students.name)
    email <- row.get(students.email)
  } yield Student(id, name, email)

  def deleteById(id: Int): F[Either[DALError, Int]] =
    Delete(students)
      .where(students.id === id)
      .execute()

  def create(student: Student): F[Either[DALError, Int]] =
    Insert
      .into(students)
      .set(
        students.id -> student.id,
        students.name -> student.name,
        students.email -> student.email
      )
      .execute()

  def update(student: Student): F[Either[DALError, Int]] =
    Update(students)
      .set(
        students.name -> student.name,
        students.email -> student.email
      )
      .where(students.id === student.id)
      .execute()

  def selectById(id: Int): F[Either[DALError, Option[Student]]] =
    Select
      .from(students)
      .take(students.id, students.name, students.email)
      .where(students.id === id)
      .sortBy(students.id.asc)
      .inRange(0, 1)
      .fetchOne()

  def selectByName(name: String): F[Either[DALError, Seq[Student]]] =
    Select
      .from(students)
      .take(students.*)
      .where(students.name like name)
      .sortBy(students.id.asc)
      .fetchMany()

  def selectByEmail(email: String): F[Either[DALError, Seq[Student]]] =
    Select
      .from(students)
      .take(students.*)
      .where(students.email like email)
      .sortBy(students.id.asc)
      .fetchMany()

  def selectByMissingEmail(): F[Either[DALError, Seq[Student]]] =
    Select
      .from(students)
      .take(students.*)
      .where(students.email.isNotDefined)
      .sortBy(students.id.asc)
      .fetchMany()

  def selectByIds(ids: Int*): F[Either[DALError, Seq[Student]]] =
    Select
      .from(students)
      .take(students.*)
      .where(students.id in ids)
      .sortBy(students.id.asc)
      .fetchMany()
}
