import cats.Applicative
import cats.implicits._
import io.epifab.dal.domain._
import io.epifab.dal.{QueryRunner, Row}

import scala.language.higherKinds

case class Student(id: Int, name: String, email: String)

class StudentsRepo[F[_]](queryRunner: QueryRunner[F])(implicit a: Applicative[F]) {
  import Schema._
  import io.epifab.dal.Implicits._

  implicit private val extractStudent: Row => Either[ExtractorError, Student] = row => for {
    id <- row.get(students.id)
    name <- row.get(students.name)
    email <- row.get(students.email)
  } yield Student(id, name, email)

  def deleteById(id: Int): F[Either[DALError, Int]] = {
    val query = Delete(students)
      .where(students.id === id)

    queryRunner.run(query)
  }

  def create(student: Student): F[Either[DALError, Int]] = {
    val query = Insert
      .into(students)
      .set(
        students.id -> student.id,
        students.name -> student.name,
        students.email -> student.email
      )

    queryRunner.run(query)
  }

  def update(student: Student): F[Either[DALError, Int]] = {
    val query = Update(students)
      .set(
        students.name -> student.name,
        students.email -> student.email
      )
      .where(students.id === student.id)

    queryRunner.run(query)
  }

  def selectById(id: Int): F[Either[DALError, Option[Student]]] = {
    val query = Select
      .from(students)
      .take(students.id, students.name, students.email)
      .where(students.id === id)
      .sortBy(students.id.asc)

    queryRunner.run(query)
      .map(_.map(_.headOption))
  }

  def selectByName(name: String): F[Either[DALError, Seq[Student]]] = {
    val query = Select
      .from(students)
      .take(students.id, students.name, students.email)
      .where(students.name like name)
      .sortBy(students.id.asc)

    queryRunner.run(query)
  }

  def selectByIds(ids: Int*): F[Either[DALError, Seq[Student]]] = {
    val query = Select
      .from(students)
      .take(students.id, students.name, students.email)
      .where(students.id in ids)
      .sortBy(students.id.asc)

    queryRunner.run(query)
  }
}
