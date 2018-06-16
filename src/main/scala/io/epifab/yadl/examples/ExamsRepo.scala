package io.epifab.yadl.examples

import cats.Applicative
import io.epifab.yadl.domain._
import io.epifab.yadl.implicits._
import shapeless._

import scala.language.higherKinds

class ExamsRepo[F[_]](implicit queryRunner: QueryRunner[F], a: Applicative[F]) {
  private lazy val exams = new Schema.ExamsTable("e")

  implicit private val examExtractor: Extractor[Exam] = row => for {
    rate <- row.get(exams.rate)
    courseId <- row.get(exams.courseId)
    studentId <- row.get(exams.studentId)
  } yield Exam(studentId, courseId, rate)

  implicit private val courseExtractor: Extractor[Course] = row => for {
    id <- row.get(exams.courseId)
    name <- row.get(exams.course.name)
  } yield Course(id, name)

  implicit private val examCourseExtractor: Extractor[Exam :: Course :: HNil] = row => for {
    exam <- examExtractor(row)
    course <- courseExtractor(row)
  } yield exam :: course :: HNil

  def selectByStudentId(studentId: Int): F[Either[DALError, Seq[Exam :: Course :: HNil]]] =
    Select
      .from(exams)
      .innerJoin(exams.course)
      .take(exams.*)
      .take(exams.course.*)
      .where(exams.studentId === studentId)
      .fetchMany()

  def createExam(exam: Exam): F[Either[DALError, Int]] =
    Insert
      .into(exams)
      .set(
        exams.studentId -> exam.studentId,
        exams.courseId -> exam.courseId,
        exams.rate -> exam.rate
      )
      .execute()
}
