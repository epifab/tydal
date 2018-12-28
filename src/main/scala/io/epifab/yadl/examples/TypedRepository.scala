package io.epifab.yadl.examples

import cats.Applicative
import io.epifab.yadl.domain._
import io.epifab.yadl.implicits._
import shapeless.{HNil, ::}

class TypedRepository[F[_]: Applicative](implicit val queryRunner: QueryRunner[F]) {
  private val studentsDS = new Schema.StudentsTable
  private val examsDS = new Schema.ExamsTable
  private val coursesDS = new Schema.CoursesTable

  private def studentsRepr(students: Schema.StudentsTable) =
    (students.id +:
      students.name +:
      students.email +:
      students.dateOfBirth +:
      students.address +:
      students.interests +:
      SNil).as[Student]

  private def coursesRepr(courses: Schema.CoursesTable) =
    (courses.id +:
      courses.name +:
      SNil).as[Course]

  private def examsRepr(exams: Schema.ExamsTable) =
    (exams.studentId +:
      exams.courseId +:
      exams.score +:
      exams.dateTime +:
      SNil).as[Exam]

  def findStudent(id: Int): F[Either[DALError, Option[Student]]] =
    TypedSelect
      .from(studentsDS)
      .take(studentsRepr(studentsDS))
      .where(studentsDS.id === Value(id))
      .sortBy(studentsDS.id.asc)
      .inRange(0, 1)
      .fetchOne

  def findExamsByStudentId(studentId: Int): F[Either[DALError, Seq[Exam :: Course :: HNil]]] =
    TypedSelect
      .from(examsDS)
      .innerJoin(examsDS.course)
      .take(examsRepr(examsDS) ++: coursesRepr(examsDS.course) ++: SNil)
      .where(examsDS.studentId === Value(studentId))
      .sortBy(examsDS.courseId.asc)
      .fetchMany

  def createStudent(student: Student): F[Either[DALError, Int]] =
    Insert
      .into(studentsDS)
      .set(
        studentsDS.id -> student.id,
        studentsDS.name -> student.name,
        studentsDS.email -> student.email,
        studentsDS.dateOfBirth -> student.dateOfBirth,
        studentsDS.address -> student.address,
        studentsDS.interests -> student.interests
      )
      .execute()

  def createExam(exam: Exam): F[Either[DALError, Int]] =
    Insert
      .into(examsDS)
      .set(
        examsDS.studentId -> exam.studentId,
        examsDS.courseId -> exam.courseId,
        examsDS.score -> exam.score,
        examsDS.dateTime -> exam.dateTime
      )
      .execute()

  def createCourse(course: Course): F[Either[DALError, Int]] =
    Insert.into(coursesDS)
      .set(
        coursesDS.id -> course.id,
        coursesDS.name -> course.name
      )
      .execute()
}
