import java.sql.{Connection, DriverManager}

import cats.data.EitherT
import io.epifab.dal.domain.DALError
import io.epifab.dal.{PostgresQueryBuilders, PostgresQueryRunner}
import org.scalatest.{BeforeAndAfterAll, FlatSpec}
import org.scalatest.Matchers._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class PostgresQueryRunnerTest extends FlatSpec with BeforeAndAfterAll {
  import cats.implicits._

  import scala.concurrent.ExecutionContext.Implicits.global

  val student1 = Student(1, "John Doe", "john@doe.com")
  val student2 = Student(2, "Jane Doe", "jane@doe.com")
  val student3 = Student(3, "Jack Roe", "jack@roe.com")

  val connection: Connection = DriverManager
    .getConnection(
      s"jdbc:postgresql://${sys.env("DB_HOST")}/${sys.env("DB_NAME")}?user=${sys.env("DB_USER")}&password=${sys.env("DB_PASS")}")

  val studentLayer = new StudentsRepo[Future](
    new PostgresQueryRunner(
      connection,
      PostgresQueryBuilders.build
    )
  )

  override def beforeAll(): Unit = {
    val fe: EitherT[Future, DALError, Unit] = for {
      _ <- EitherT(studentLayer.create(student1))
      _ <- EitherT(studentLayer.create(student2))
      _ <- EitherT(studentLayer.create(student3))
    } yield {}

    Await.result(fe.value, 3.seconds)
  }

  override def afterAll(): Unit = {
    val fe: EitherT[Future, DALError, Unit] = for {
      _ <- EitherT(studentLayer.deleteById(2))
      _ <- EitherT(studentLayer.deleteById(3))
      _ <- EitherT(studentLayer.deleteById(1))
    } yield {}

    Await.result(fe.value, 3.seconds)
  }

  "The query runner" should "retrieve a student by ID" in {
    Await.result(studentLayer.selectById(2), 3.seconds) shouldBe Right(Some(student2))
  }

  it should "retrieve a list of students by name" in {
    Await.result(studentLayer.selectByName("%Doe"), 3.seconds) shouldBe Right(Seq(
      student1,
      student2
    ))
  }

  it should "retrieve a list of students by ids" in {
    Await.result(studentLayer.selectByIds(2, 3, 4), 3.seconds) shouldBe Right(Seq(student2, student3))
  }

  it should "update a student" in {
    val edited: EitherT[Future, DALError, Option[Student]] = for {
      _ <- EitherT(studentLayer.update(student3.copy(name = "Edited")))
      edited <- EitherT(studentLayer.selectById(3))
    } yield edited

    Await.result(edited.value, 3.seconds).map(_.map(_.name)) shouldBe Right(Some("Edited"))
  }
}
