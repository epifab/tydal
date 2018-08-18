package io.epifab.yadl.examples

import io.epifab.yadl.domain._
import io.epifab.yadl.examples.Schema.{ExamsSubQuery, ExamsTable}
import io.epifab.yadl.implicits._

import scala.language.higherKinds

trait StudentsRepo[F[_]] extends Repo[F] {
  object Students extends Schema.StudentsTable

  implicit private val studentExtractor: Extractor[Student] = row => for {
    id <- row.get(Students.id)
    name <- row.get(Students.name)
    email <- row.get(Students.email)
    dateOfBirth <- row.get(Students.dateOfBirth)
    address <- row.get(Students.address)
    interests <- row.get(Students.interests)
  } yield Student(id, name, email, dateOfBirth, address.map(_.value), interests)

  implicit private val examsExtractor: Extractor[StudentExams] = row => for {
    id <- row.get(Students.examsProjection.studentId)
    count <- row.get(Students.examsProjection.count)
    avgScore <- row.get(Students.examsProjection.avgScore)
    minScore <- row.get(Students.examsProjection.minScore)
    maxScore <- row.get(Students.examsProjection.maxScore)
  } yield StudentExams(id, count, avgScore, minScore, maxScore)

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
        Students.email -> student.email,
        Students.dateOfBirth -> student.dateOfBirth,
        Students.address -> student.address.map(Json(_)),
        Students.interests -> student.interests
      )
      .execute()

  def updateStudent(student: Student): F[Either[DALError, Int]] =
    Update(Students)
      .set(
        Students.name -> student.name,
        Students.email -> student.email,
        Students.dateOfBirth -> student.dateOfBirth,
        Students.address -> student.address.map(Json(_)),
        Students.interests -> student.interests
      )
      .where(Students.id === Value(student.id))
      .execute()

  def findStudent(id: Int): F[Either[DALError, Option[Student]]] =
    Select
      .from(Students)
      .take(Students.*)
      .where(Students.id === Value(id))
      .sortBy(Students.id.asc)
      .inRange(0, 1)
      .fetchOne()

  def findStudentsByInterests(interests: Seq[String]): F[Either[DALError, Seq[Student]]] =
    Select
      .from(Students)
      .take(Students.*)
      .where(Students.interests contains Value(interests))
      .sortBy(Students.id.asc)
      .fetchMany()

  def findStudentsByAnyInterest(interests: Seq[String]): F[Either[DALError, Seq[Student]]] =
    Select
      .from(Students)
      .take(Students.*)
      .where(Students.interests overlaps Value(interests))
      .sortBy(Students.id.asc)
      .fetchMany()

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
      .fetchMany()

  def findStudentExamStats(id: Int): F[Either[DALError, Option[StudentExams]]] =
    Select
      .from(Students.examsProjection.exams)
      .take(Students.examsProjection.studentId)
      .aggregateBy(
        Students.examsProjection.count,
        Students.examsProjection.avgScore,
        Students.examsProjection.minScore,
        Students.examsProjection.maxScore
      )
      .where(Students.examsProjection.studentId === Value(id))
      .fetchOne()

  def findBestStudents: F[Either[DALError, Seq[Student]]] = {
    val avgStudent = new ExamsSubQuery

    Select
      .from(Students)
      .take(Students.*)
      .innerJoin(Students.examsProjection.exams, Students.examsProjection.studentId === Students.id)
      .innerJoin(avgStudent, avgStudent.avgScore <= Students.examsProjection.avgScore)
      .inRange(0, 10)
      .fetchMany()
  }
}
