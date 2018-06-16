package io.epifab.yadl.examples

import cats.Applicative
import cats.implicits._
import io.epifab.yadl.domain._
import io.epifab.yadl.implicits._

import scala.language.higherKinds

class StudentsRepo[F[_]](queryRunner: QueryRunner[F])(implicit a: Applicative[F]) {
  private lazy val students = new Schema.StudentsTable("s")

  implicit private val studentExtractor: Extractor[Student] = row => for {
    id <- row.get(students.id)
    name <- row.get(students.name)
    email <- row.get(students.email)
  } yield Student(id, name, email)

  def deleteById(id: Int): F[Either[DALError, Int]] = {
    val query = Delete(students)
      .where(students.id === id)

    queryRunner.run(query)
  }

  def create(student: Student): F[Either[DALError, Int]] = {
    val query = Insert
      .into(students)
      .set(
        students.id -> student.id,
        students.name -> student.name,
        students.email -> student.email
      )

    queryRunner.run(query)
  }

  def update(student: Student): F[Either[DALError, Int]] = {
    val query = Update(students)
      .set(
        students.name -> student.name,
        students.email -> student.email
      )
      .where(students.id === student.id)

    queryRunner.run(query)
  }

  def selectById(id: Int): F[Either[DALError, Option[Student]]] = {
    val query = Select
      .from(students)
      .take(students.id, students.name, students.email)
      .where(students.id === id)
      .sortBy(students.id.asc)
      .inRange(0, 1)

    queryRunner.run(query)
      .map(_.map(_.headOption))
  }

  def selectByName(name: String): F[Either[DALError, Seq[Student]]] = {
    val query = Select
      .from(students)
      .take(students.id, students.name, students.email)
      .where(students.name like name)
      .sortBy(students.id.asc)

    queryRunner.run(query)
  }

  def selectByEmail(email: String): F[Either[DALError, Seq[Student]]] = {
    val query = Select
      .from(students)
      .take(students.id, students.name, students.email)
      .where(students.email like email)
      .sortBy(students.id.asc)

    queryRunner.run(query)
  }

  def selectByMissingEmail(): F[Either[DALError, Seq[Student]]] = {
    val query = Select
      .from(students)
      .take(students.id, students.name, students.email)
      .where(students.email.isNotDefined)
      .sortBy(students.id.asc)

    queryRunner.run(query)
  }

  def selectByIds(ids: Int*): F[Either[DALError, Seq[Student]]] = {
    val query = Select
      .from(students)
      .take(students.id, students.name, students.email)
      .where(students.id in ids)
      .sortBy(students.id.asc)

    queryRunner.run(query)
  }
}
