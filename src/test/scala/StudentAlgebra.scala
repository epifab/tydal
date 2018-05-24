import cats.Applicative
import cats.implicits._
import domain._

import scala.language.higherKinds


case class Student(id: Int, name: String)


trait StudentAlgebra[F[_]] {
  import Filter._
  implicit val a: Applicative[F]

  object students extends Table("students", "s") {
    lazy val id: TableField[Int] = field("id")
    lazy val name: TableField[String] = field("name")

    object exams extends Table("exams", "e") {
      lazy val studentId: TableField[Int] = field("student_id")
      lazy val rate: TableField[Int] = field("rate")
    }
  }

  def selectQueryBuilder: QueryBuilder[SelectQuery]
  def selectQueryRunner: QueryRunner[F]

  def selectById(id: Int): F[Option[Student]] = {
    val query = SelectQuery
      .from(students)
      .take(students.id)
      .where(students.id === id)

    selectQueryRunner.select(query)
      .map((rows: Iterable[Row]) => rows.headOption.map(
        row => for {
          id <- row.get(students.id)
          name <- row.get(students.name)
        } yield Student(id, name)
      ))
      .map {
        case Some(Right(s)) => Some(s)
        case Some(Left(e)) => None  // Some error
        case None => None
      }
  }
}
