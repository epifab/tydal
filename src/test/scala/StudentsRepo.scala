import cats.Applicative
import cats.implicits._
import io.epifab.dal.domain._
import io.epifab.dal.{QueryRunner, Row}

import scala.language.higherKinds


case class Student(id: Int, name: String, email: String)


class StudentsRepo[F[_]](selectQueryRunner: QueryRunner[F])(implicit a: Applicative[F]) {
  import Schema._

  private def extractStudent(row: Row): Either[ExtractorError, Student] = for {
    id <- row.get(students.id)
    name <- row.get(students.name)
    email <- row.get(students.email)
  } yield Student(id, name, email)

  def selectById(id: Int): F[Either[DALError, Option[Student]]] = {
    import Filter._

    val query = SelectQuery
      .from(students)
      .take(students.id, students.name, students.email)
      .where(students.id === id)

    selectQueryRunner.select(query)
      .map {
        case Right(rows) =>
          rows.headOption.map(extractStudent) match {
            case Some(Left(error)) => Left(error)
            case Some(Right(student)) => Right(Some(student))
            case None => Right(None)
          }
        case Left(error) => Left(error)
      }
  }
}
