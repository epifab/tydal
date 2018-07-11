package io.epifab.yadl.examples

import io.epifab.yadl.domain._
import io.epifab.yadl.implicits._

import scala.language.higherKinds

trait StudentsRepo[F[_]] extends Repo[F] {
  object Students extends Schema.StudentsTable("s")

  object ExamsView {
    val table = new Schema.ExamsTable("e")

    val studentId: TableColumn[Int] = table.studentId
    val count: AggregateColumn[Int, Option[Int]] = Count(table.courseId)
    val avgScore: AggregateColumn[Int, Option[Double]] = Avg(table.score)
    val minScore: AggregateColumn[Int, Option[Int]] = Min(table.score)
    val maxScore: AggregateColumn[Int, Option[Int]] = Max(table.score)
  }

  implicit private val studentExtractor: Extractor[Student] = row => for {
    id <- row.get(Students.id)
    name <- row.get(Students.name)
    email <- row.get(Students.email)
    dateOfBirth <- row.get(Students.dateOfBirth)
    address <- row.get(Students.address)
    interests <- row.get(Students.interests)
  } yield Student(id, name, email, dateOfBirth, address.map(_.value), interests)

  implicit private val examsExtractor: Extractor[StudentExams] = row => for {
    id <- row.get(ExamsView.studentId)
    count <- row.get(ExamsView.count)
    avgScore <- row.get(ExamsView.avgScore)
    minScore <- row.get(ExamsView.minScore)
    maxScore <- row.get(ExamsView.maxScore)
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
      .from(ExamsView.table)
      .take(ExamsView.studentId)
      .aggregateBy(
        ExamsView.count,
        ExamsView.avgScore,
        ExamsView.minScore,
        ExamsView.maxScore
      )
      .where(ExamsView.studentId === Value(id))
      .fetchOne()
}
