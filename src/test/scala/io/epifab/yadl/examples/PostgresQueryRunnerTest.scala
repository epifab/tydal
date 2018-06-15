package io.epifab.yadl.examples

import java.sql.{Connection, DriverManager}

import cats.data.EitherT
import io.epifab.yadl.domain.DALError
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, FlatSpec}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class PostgresQueryRunnerTest extends FlatSpec with BeforeAndAfterAll {
  import cats.implicits._
  import io.epifab.yadl.postgres._

  import scala.concurrent.ExecutionContext.Implicits.global

  implicit class ExtendedFuture[T](f: Future[T]) {
    def eventually: T = Await.result[T](f, 5.seconds)
  }

  val student1 = Student(1, "John Doe", Some("john@doe.com"))
  val student2 = Student(2, "Jane Doe", Some("jane@doe.com"))
  val student3 = Student(3, "Jack Roe", None)

  val connection: Connection = DriverManager
    .getConnection(
      s"jdbc:postgresql://${sys.env("DB_HOST")}/${sys.env("DB_NAME")}?user=${sys.env("DB_USER")}&password=${sys.env("DB_PASS")}")

  val studentsRepo = new StudentsRepo[Future](asyncQueryRunner(connection))

  override def beforeAll(): Unit = {
    val fe: EitherT[Future, DALError, Unit] = for {
      _ <- EitherT(studentsRepo.create(student1))
      _ <- EitherT(studentsRepo.create(student2))
      _ <- EitherT(studentsRepo.create(student3))
    } yield {}

    fe.value.eventually shouldBe 'Right
  }

  override def afterAll(): Unit = {
    val fe: EitherT[Future, DALError, Unit] = for {
      _ <- EitherT(studentsRepo.deleteById(2))
      _ <- EitherT(studentsRepo.deleteById(3))
      _ <- EitherT(studentsRepo.deleteById(1))
    } yield {}

    fe.value.eventually shouldBe 'Right
  }

  "The query runner" should "retrieve a student by ID" in {
    studentsRepo.selectById(2).eventually shouldBe Right(Some(student2))
  }

  it should "retrieve a list of students by name" in {
    studentsRepo.selectByName("%Doe").eventually shouldBe Right(Seq(student1, student2))
  }

  it should "retrieve a list of students by ids" in {
    studentsRepo.selectByIds(2, 3, 4).eventually shouldBe Right(Seq(student2, student3))
  }

  it should "retrieve a list of students by email" in {
    studentsRepo.selectByEmail("%@doe.com").eventually shouldBe Right(Seq(student1, student2))
  }

  it should "retrieve students with missing email" in {
    studentsRepo.selectByMissingEmail().eventually shouldBe Right(Seq(student3))
  }

  it should "update a student" in {
    val edited: EitherT[Future, DALError, Option[Student]] = for {
      _ <- EitherT(studentsRepo.update(student3.copy(name = "Edited")))
      edited <- EitherT(studentsRepo.selectById(3))
    } yield edited

    edited.value.eventually.map(_.map(_.name)) shouldBe Right(Some("Edited"))
  }
}
