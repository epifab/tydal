package io.epifab.yadl.examples

import java.time.LocalDate

import io.epifab.yadl.domain._
import io.epifab.yadl.implicits._

import scala.language.higherKinds

trait StudentsRepo[F[_]] extends Repo[F] {
  import Adapters._

  val studentsDS = new Schema.StudentsTable

  def deleteStudent(id: Int): F[Either[DALError, Int]] =
    Delete(studentsDS)
      .where(studentsDS.id === Value(id))
      .execute()

  def createStudent(student: Student): F[Either[DALError, Int]] =
    Insert
      .into(studentsDS)
      .set(student)
      .execute()

  def updateStudent(student: Student): F[Either[DALError, Int]] =
    Update(studentsDS)
      .set(student)
      .where(studentsDS.id === Value(student.id))
      .execute()

  def findStudent(id: Int): F[Either[DALError, Option[Student]]] =
    Select
      .from(studentsDS)
      .take(studentsDS.*)
      .where(studentsDS.id === Value(id))
      .sortBy(Asc(studentsDS.id))
      .inRange(0, 1)
      .fetchOne

  def findStudentsByInterests(interests: Seq[Interest]): F[Either[DALError, Seq[Student]]] =
    Select
      .from(studentsDS)
      .take(studentsDS.*)
      .where(studentsDS.interests supersetOf Value(interests))
      .sortBy(Asc(studentsDS.id))
      .fetchMany

  def findStudentsByAnyInterest(interests: Seq[Interest]): F[Either[DALError, Seq[Student]]] =
    Select
      .from(studentsDS)
      .take(studentsDS.*)
      .where(studentsDS.interests overlaps Value(interests))
      .sortBy(Asc(studentsDS.id))
      .fetchMany

  def findStudentByName(name: String): F[Either[DALError, Seq[Student]]] =
    Select
      .from(studentsDS)
      .take(studentsDS.*)
      .where(studentsDS.name like Value(name))
      .sortBy(Asc(studentsDS.id))
      .fetchMany

  def findStudentByEmail(email: String): F[Either[DALError, Seq[Student]]] =
    Select
      .from(studentsDS)
      .take(studentsDS.*)
      .where(studentsDS.email like Value(email))
      .sortBy(Asc(studentsDS.id))
      .fetchMany

  def findStudentsWithoutEmail(): F[Either[DALError, Seq[Student]]] =
    Select
      .from(studentsDS)
      .take(studentsDS.*)
      .where(studentsDS.email.isNotDefined)
      .sortBy(Asc(studentsDS.id))
      .fetchMany

  def findStudents(ids: Int*): F[Either[DALError, Seq[Student]]] =
    Select
      .from(studentsDS)
      .take(studentsDS.*)
      .where(studentsDS.id in Value(ids))
      .sortBy(Asc(studentsDS.id))
      .fetchMany

  def findStudentExamStats(id: Int): F[Either[DALError, Option[StudentExams]]] = {
    val stats = new Schema.ExamsStats(new Schema.ExamsTable)

    Select
      .from(stats)
      .take(stats.*)
      .groupBy(stats.studentId)
      .where(stats.studentId === Value(id))
      .fetchOne
  }

  def findStudentsByDateOfBirth(dates: LocalDate*): F[Either[DALError, Seq[Student]]] = {
    Select
      .from(studentsDS)
      .take(studentsDS.*)
      .where(studentsDS.dateOfBirth in Value(dates))
      .fetchMany
  }
}
