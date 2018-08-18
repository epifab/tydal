package io.epifab.yadl.examples

import java.time.LocalDate

import cats.implicits._
import io.epifab.yadl.domain._
import io.epifab.yadl.implicits._
import shapeless._

import scala.language.higherKinds

trait ExamsRepo[F[_]] extends Repo[F] {
  object Exams extends Schema.ExamsTable

  implicit private val examExtractor: Extractor[Exam] = row => for {
    score <- row.get(Exams.score)
    courseId <- row.get(Exams.courseId)
    studentId <- row.get(Exams.studentId)
    dateTime <- row.get(Exams.dateTime)
  } yield Exam(studentId, courseId, score, dateTime)

  implicit private val courseExtractor: Extractor[Course] = row => for {
    id <- row.get(Exams.courseId)
    name <- row.get(Exams.course.name)
  } yield Course(id, name)

  implicit private val examCourseExtractor: Extractor[Exam :: Course :: HNil] = row => for {
    exam <- examExtractor(row)
    course <- courseExtractor(row)
  } yield exam :: course :: HNil

  def findExamsByStudentId(studentId: Int): F[Either[DALError, Seq[Exam :: Course :: HNil]]] =
    Select
      .from(Exams)
      .innerJoin(Exams.course)
      .take(Exams.* ++ Exams.course.*)
      .where(Exams.studentId === Value(studentId))
      .fetchMany()

  def findExamsByDate(date: LocalDate): F[Either[DALError, Seq[Exam :: Course ::HNil]]] =
    Select
      .from(Exams)
      .innerJoin(Exams.course)
      .take(Exams.* ++ Exams.course.*)
      .where(Exams.dateTime >= Value(date.atStartOfDay) and Exams.dateTime < Value(date.plusDays(1).atStartOfDay))
      .sortBy(Exams.studentId.asc)
      .fetchMany()

  def findStudentsExams(students: Student*): F[Either[DALError, Iterable[Student :: Seq[Exam :: Course :: HNil] :: HNil]]] = {
    val examsFE = Select
      .from(Exams)
      .innerJoin(Exams.course)
      .take(Exams.* ++ Exams.course.*)
      .where(Exams.studentId in Value(students.map(_.id)))
      .fetchMany[Exam :: Course :: HNil]()

    examsFE.map(_.map(
      exams =>
        students.map(student =>
          student :: exams.filter(_.select[Exam].studentId == student.id) :: HNil
        )
      )
    )
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
