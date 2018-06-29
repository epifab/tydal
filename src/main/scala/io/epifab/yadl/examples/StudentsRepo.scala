package io.epifab.yadl.examples

import io.epifab.yadl.domain._
import io.epifab.yadl.implicits._

import scala.language.higherKinds

trait StudentsRepo[F[_]] extends Repo[F] {
  object Students extends Schema.StudentsTable("s")

  implicit private val studentExtractor: Extractor[Student] = row => for {
    id <- row.get(Students.id)
    name <- row.get(Students.name)
    email <- row.get(Students.email)
  } yield Student(id, name, email)

  def deleteStudent(id: Int): F[Either[DALError, Int]] =
    Delete(Students)
      .where(Students.id === Value(id))
      .execute()

  def createStudent(student: Student): F[Either[DALError, Int]] =
    Insert
      .into(Students)
      .set(
        Students.id -> student.id,
        Students.name -> student.name,
        Students.email -> student.email
      )
      .execute()

  def updateStudent(student: Student): F[Either[DALError, Int]] =
    Update(Students)
      .set(
        Students.name -> student.name,
        Students.email -> student.email
      )
      .where(Students.id === Value(student.id))
      .execute()

  def findStudent(id: Int): F[Either[DALError, Option[Student]]] =
    Select
      .from(Students)
      .take(Students.id, Students.name, Students.email)
      .where(Students.id === Value(id))
      .sortBy(Students.id.asc)
      .inRange(0, 1)
      .fetchOne()

  def findStudentByName(name: String): F[Either[DALError, Seq[Student]]] =
    Select
      .from(Students)
      .take(Students.*)
      .where(Students.name like Value(name))
      .sortBy(Students.id.asc)
      .fetchMany()

  def findStudentByEmail(email: String): F[Either[DALError, Seq[Student]]] =
    Select
      .from(Students)
      .take(Students.*)
      .where(Students.email like Value(email))
      .sortBy(Students.id.asc)
      .fetchMany()

  def findStudentsWithoutEmail(): F[Either[DALError, Seq[Student]]] =
    Select
      .from(Students)
      .take(Students.*)
      .where(Students.email.isNotDefined)
      .sortBy(Students.id.asc)
      .fetchMany()

  def findStudents(ids: Int*): F[Either[DALError, Seq[Student]]] =
    Select
      .from(Students)
      .take(Students.*)
      .where(Students.id in Value(ids))
      .sortBy(Students.id.asc)
      .fetchMany[Student]()
}
