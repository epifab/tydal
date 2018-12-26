package io.epifab.yadl.examples

import java.time.LocalDate

import io.epifab.yadl.domain._
import io.epifab.yadl.examples.Schema.{ExamsProjection, ExamsSubquery, StudentsTable}
import io.epifab.yadl.implicits._

import scala.language.higherKinds

trait StudentsRepo[F[_]] extends Repo[F] {
  import Adapters._

  val studentDataSource = new Schema.StudentsTable

  private def studentExtractor(studentDataSource: StudentsTable): Extractor[Student] = row => for {
    id <- row.get(studentDataSource.id)
    name <- row.get(studentDataSource.name)
    email <- row.get(studentDataSource.email)
    dateOfBirth <- row.get(studentDataSource.dateOfBirth)
    address <- row.get(studentDataSource.address)
    interests <- row.get(studentDataSource.interests)
  } yield Student(id, name, email, dateOfBirth, address, interests)

  private def examsProjectionsExtractor(exams: ExamsProjection): Extractor[StudentExams] = row => for {
    count <- row.get(exams.examsCount)
    avgScore <- row.get(exams.avgScore)
    minScore <- row.get(exams.minScore)
    maxScore <- row.get(exams.maxScore)
  } yield StudentExams(count, avgScore, minScore, maxScore)

  def deleteStudent(id: Int): F[Either[DALError, Int]] =
    Delete(studentDataSource)
      .where(studentDataSource.id === Value(id))
      .execute()

  def createStudent(student: Student): F[Either[DALError, Int]] =
    Insert
      .into(studentDataSource)
      .set(
        studentDataSource.id -> student.id,
        studentDataSource.name -> student.name,
        studentDataSource.email -> student.email,
        studentDataSource.dateOfBirth -> student.dateOfBirth,
        studentDataSource.address -> student.address,
        studentDataSource.interests -> student.interests
      )
      .execute()

  def updateStudent(student: Student): F[Either[DALError, Int]] =
    Update(studentDataSource)
      .set(
        studentDataSource.name -> student.name,
        studentDataSource.email -> student.email,
        studentDataSource.dateOfBirth -> student.dateOfBirth,
        studentDataSource.address -> student.address,
        studentDataSource.interests -> student.interests
      )
      .where(studentDataSource.id === Value(student.id))
      .execute()

  def findStudent(id: Int): F[Either[DALError, Option[Student]]] =
    Select
      .from(studentDataSource)
      .take(studentDataSource.*)
      .where(studentDataSource.id === Value(id))
      .sortBy(studentDataSource.id.asc)
      .inRange(0, 1)
      .fetchOne(studentExtractor(studentDataSource))

  def findStudentsByInterests(interests: Seq[Interest]): F[Either[DALError, Seq[Student]]] =
    Select
      .from(studentDataSource)
      .take(studentDataSource.*)
      .where(studentDataSource.interests contains Value(interests))
      .sortBy(studentDataSource.id.asc)
      .fetchMany(studentExtractor(studentDataSource))

  def findStudentsByAnyInterest(interests: Seq[Interest]): F[Either[DALError, Seq[Student]]] =
    Select
      .from(studentDataSource)
      .take(studentDataSource.*)
      .where(studentDataSource.interests overlaps Value(interests))
      .sortBy(studentDataSource.id.asc)
      .fetchMany(studentExtractor(studentDataSource))

  def findStudentByName(name: String): F[Either[DALError, Seq[Student]]] =
    Select
      .from(studentDataSource)
      .take(studentDataSource.*)
      .where(studentDataSource.name like Value(name))
      .sortBy(studentDataSource.id.asc)
      .fetchMany(studentExtractor(studentDataSource))

  def findStudentByEmail(email: String): F[Either[DALError, Seq[Student]]] =
    Select
      .from(studentDataSource)
      .take(studentDataSource.*)
      .where(studentDataSource.email like Value(email))
      .sortBy(studentDataSource.id.asc)
      .fetchMany(studentExtractor(studentDataSource))

  def findStudentsWithoutEmail(): F[Either[DALError, Seq[Student]]] =
    Select
      .from(studentDataSource)
      .take(studentDataSource.*)
      .where(studentDataSource.email.isNotDefined)
      .sortBy(studentDataSource.id.asc)
      .fetchMany(studentExtractor(studentDataSource))

  def findStudents(ids: Int*): F[Either[DALError, Seq[Student]]] =
    Select
      .from(studentDataSource)
      .take(studentDataSource.*)
      .where(studentDataSource.id in Value(ids))
      .sortBy(studentDataSource.id.asc)
      .fetchMany(studentExtractor(studentDataSource))

  def findStudentExamStats(id: Int): F[Either[DALError, Option[StudentExams]]] = {
    val examsProjections = ExamsProjection()

    Select
      .from(examsProjections)
      .take(examsProjections.studentId)
      .aggregateBy(
        examsProjections.examsCount,
        examsProjections.avgScore,
        examsProjections.minScore,
        examsProjections.maxScore
      )
      .where(examsProjections.studentId === Value(id))
      .fetchOne(examsProjectionsExtractor(examsProjections))
  }

  def findStudentExamStats2(id: Int): F[Either[DALError, Option[(Int, Option[Double])]]] = {
    val examsSubquery = new ExamsSubquery

    Select
      .from(studentDataSource)
      .innerJoin(examsSubquery on (_.studentId === studentDataSource.id))
      .take(
        studentDataSource.id,
        examsSubquery.avgScore
      )
      .where(examsSubquery.studentId === Value(id))
      .fetchOne(
        row => for {
          studentId <- row.get(studentDataSource.id)
          avgScore <- row.get(examsSubquery.avgScore)
        } yield (studentId, avgScore)
      )
  }

  def findStudentsByDateOfBirth(dates: LocalDate*): F[Either[DALError, Seq[Student]]] = {
    Select
      .from(studentDataSource)
      .take(studentDataSource.*)
      .where(studentDataSource.dateOfBirth in Value(dates))
      .fetchMany[Student](studentExtractor(studentDataSource))
  }
}
