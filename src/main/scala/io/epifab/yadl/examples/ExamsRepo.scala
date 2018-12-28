package io.epifab.yadl.examples

import java.time.{LocalDate, LocalDateTime}

import cats.implicits._
import io.epifab.yadl.domain._
import io.epifab.yadl.implicits._
import shapeless.{::, HNil}

import scala.language.higherKinds

trait ExamsRepo[F[_]] extends Repo[F] {
  object Exams extends Schema.ExamsTable

  def findExamsByStudentId(studentId: Int): F[Either[DALError, Seq[Exam :: Course :: HNil]]] = {
    TypedSelect
      .from(Exams)
      .innerJoin(Exams.course)
      .where(Exams.studentId === Value(studentId))
      .take(Exams.* ++: Exams.course.* ++: SNil)
      .fetchMany
  }

  def findExamsByDate(date: LocalDate): F[Either[DALError, Seq[Exam :: Course ::HNil]]] =
    TypedSelect
      .from(Exams)
      .innerJoin(Exams.course)
      .take(Exams.* ++: Exams.course.* ++: SNil)
      .where(Exams.dateTime >= Value(date.atStartOfDay) and Exams.dateTime < Value(date.plusDays(1).atStartOfDay))
      .sortBy(Exams.studentId.asc)
      .fetchMany

  def findStudentsExams(students: Student*): F[Either[DALError, Iterable[Student :: Seq[Exam :: Course :: HNil] :: HNil]]] = {
    val examsFE = TypedSelect
      .from(Exams)
      .innerJoin(Exams.course)
      .take(Exams.* ++: Exams.course.* ++: SNil)
      .where(Exams.studentId in Value(students.map(_.id)))
      .sortBy(Exams.course.id.asc)
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
    TypedSelect
      .from(Exams)
      .take(Exams.*)
      .where(Exams.dateTime in Value(dates))
      .fetchMany
  }

  def createExam(exam: Exam): F[Either[DALError, Int]] =
    Insert
      .into(Exams)
      .set(
        Exams.studentId -> exam.studentId,
        Exams.courseId -> exam.courseId,
        Exams.score -> exam.score,
        Exams.dateTime -> exam.dateTime
      )
      .execute()
}
