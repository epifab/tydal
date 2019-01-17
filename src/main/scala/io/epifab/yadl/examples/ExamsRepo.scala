package io.epifab.yadl.examples

import java.time.{LocalDate, LocalDateTime}

import cats.implicits._
import io.epifab.yadl.domain._
import io.epifab.yadl.implicits._
import shapeless.{::, HNil}

import scala.language.higherKinds

trait ExamsRepo[F[_]] extends Repo[F] {
  private val examsDS = new Schema.ExamsTable

  def findExamsByStudentId(studentId: Int): F[Either[DALError, Seq[Exam :: Course :: HNil]]] = {
    Select
      .from(examsDS)
      .innerJoin(examsDS.course)
      .where(examsDS.studentId === Value(studentId))
      .take(Terms(examsDS.* :: examsDS.course.* :: HNil))
      .fetchMany
  }

  def findExamsByDate(date: LocalDate): F[Either[DALError, Seq[Exam :: Course ::HNil]]] =
    Select
      .from(examsDS)
      .innerJoin(examsDS.course)
      .take(Terms(examsDS.* :: examsDS.course.* :: HNil))
      .where(examsDS.dateTime >= Value(date.atStartOfDay) and examsDS.dateTime < Value(date.plusDays(1).atStartOfDay))
      .sortBy(examsDS.studentId.asc)
      .fetchMany

  def findStudentsExams(students: Student*): F[Either[DALError, Iterable[Student :: Seq[Exam :: Course :: HNil] :: HNil]]] = {
    val examsFE = Select
      .from(examsDS)
      .innerJoin(examsDS.course)
      .take(Terms(examsDS.* :: examsDS.course.* :: HNil))
      .where(examsDS.studentId in Value(students.map(_.id)))
      .sortBy(examsDS.course.id.asc)
      .fetchMany

    examsFE.map(_.map(
      exams =>
        students.map(student =>
          student :: exams.filter(_.select[Exam].studentId == student.id) :: HNil
        )
      )
    )
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
