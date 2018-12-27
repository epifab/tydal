package io.epifab.yadl.examples

import cats.Applicative
import io.epifab.yadl.domain._
import io.epifab.yadl.implicits._

class TypedStudentsRepo[F[_]: Applicative](implicit val queryRunner: QueryRunner[F]) {
  private val studentDataSource = new Schema.StudentsTable

  private val selectable =
    (studentDataSource.id +:
      studentDataSource.name +:
      studentDataSource.email +:
      studentDataSource.dateOfBirth +:
      studentDataSource.address +:
      studentDataSource.interests +:
      SNil).as[Student]

  def findStudent(id: Int): F[Either[DALError, Option[Student]]] =
    TypedSelect
      .from(studentDataSource)
      .take(selectable)
      .where(studentDataSource.id === Value(id))
      .sortBy(studentDataSource.id.asc)
      .inRange(0, 1)
      .fetchOne

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
}
