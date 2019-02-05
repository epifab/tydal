package io.epifab.yadl.examples

import java.time.{LocalDate, LocalDateTime}

import cats.implicits._
import io.epifab.yadl.domain._
import io.epifab.yadl.implicits._

import scala.language.higherKinds

trait ExamsRepo[F[_]] extends Repo[F] {
  private val examsDS = new Schema.ExamsTable

  def findExamsByStudentId(studentId: Int): F[Either[DALError, Seq[(Exam, Course)]]] = {
    Select(examsDS.*, examsDS.course.*)
      .from(examsDS)
      .innerJoin(examsDS.course)
      .where(examsDS.studentId === Value(studentId))
      .fetchMany
  }

  def findExamsByDate(date: LocalDate): F[Either[DALError, Seq[(Exam, Course)]]] =
    Select(examsDS.*, examsDS.course.*)
      .from(examsDS)
      .innerJoin(examsDS.course)
      .where(examsDS.dateTime >= Value(date.atStartOfDay) and examsDS.dateTime < Value(date.plusDays(1).atStartOfDay))
      .sortBy(Asc(examsDS.studentId))
      .fetchMany

  def findStudentsExams(students: Student*): F[Either[DALError, Iterable[(Student, Seq[(Exam, Course)])]]] = {
    val examsFE = Select(examsDS.*, examsDS.course.*)
      .from(examsDS)
      .innerJoin(examsDS.course)
      .where(examsDS.studentId in Value(students.map(_.id)))
      .sortBy(Asc(examsDS.course.id))
      .fetchMany

    examsFE.map(_.map(
      exams =>
        students.map(student =>
          student -> exams.filter { case (exam, _) => exam.studentId == student.id }
        )
      )
    )
  }

  def findCourseIdsByStudentExams(students: Student*): F[Either[DALError, Seq[Course]]] = {
    Select
      .from(examsDS)
      .innerJoin(examsDS.course)
      .take(examsDS.course.*)
      .groupBy(examsDS.course.id, examsDS.course.name)
      .where(examsDS.studentId in Value(students.map(_.id)))
      .sortBy(Asc(examsDS.course.id))
      .fetchMany
  }

  def findExamsByDateTime(dates: LocalDateTime*): F[Either[DALError, Seq[Exam]]] = {
    Select
      .from(examsDS)
      .take(examsDS.*)
      .where(examsDS.dateTime in Value(dates))
      .fetchMany
  }

  def createExam(exam: Exam): F[Either[DALError, Int]] =
    Insert
      .into(examsDS)
      .set(exam)
      .execute()
}
