package io.epifab.yadl.examples

import cats.implicits._
import io.epifab.yadl.domain._
import io.epifab.yadl.implicits._
import shapeless._

import scala.language.higherKinds

trait ExamsRepo[F[_]] extends Repo[F] {
  object Exams extends Schema.ExamsTable("e")

  implicit private val examExtractor: Extractor[Exam] = row => for {
    rate <- row.get(Exams.rate)
    courseId <- row.get(Exams.courseId)
    studentId <- row.get(Exams.studentId)
  } yield Exam(studentId, courseId, rate)

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
      .where(Exams.studentId === studentId)
      .fetchMany()

  def findStudentsExams(students: Student*): F[Either[DALError, Iterable[Student :: Seq[Exam :: Course :: HNil] :: HNil]]] = {
    Select
      .from(Exams)
      .innerJoin(Exams.course)
      .take(Exams.* ++ Exams.course.*)
      .where(Exams.studentId in students.map(_.id))
      .fetchMany[Exam :: Course :: HNil]()
      .map(_.map { exams =>
        exams
          .groupBy(_.head.studentId)
          .flatMap { case (studentId, studentExams) =>
            students
              .find(_.id == studentId)
              .map(student =>
                student :: studentExams :: HNil)
          }
      })
  }

  def createExam(exam: Exam): F[Either[DALError, Int]] =
    Insert
      .into(Exams)
      .set(
        Exams.studentId -> exam.studentId,
        Exams.courseId -> exam.courseId,
        Exams.rate -> exam.rate
      )
      .execute()
}
