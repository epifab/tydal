import java.sql.{Connection, DriverManager}

import io.epifab.dal.{PostgresQueryBuilders, PostgresQueryRunner}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class PostgresQueryRunnerTest extends FlatSpec {
  import cats.implicits._

  import scala.concurrent.ExecutionContext.Implicits.global

  val connection: Connection = DriverManager
    .getConnection(
      s"jdbc:postgresql://${sys.env("DB_HOST")}/${sys.env("DB_NAME")}?user=${sys.env("DB_USER")}&password=${sys.env("DB_PASS")}")

  val studentLayer = new StudentsRepo[Future](
    new PostgresQueryRunner(
      connection,
      PostgresQueryBuilders.select
    )
  )

  "The query runner" should "retrieve a student by ID" in {
    Await.result(studentLayer.selectById(2), 3.seconds) shouldBe Right(Some(Student(2, "Jane Doe", "jane@doe.com")))
  }

  it should "retrieve a list of students by name" in {
    Await.result(studentLayer.selectByName("%Doe"), 3.seconds) shouldBe Right(Seq(
      Student(1, "John Doe", "john@doe.com"),
      Student(2, "Jane Doe", "jane@doe.com")
    ))
  }

  it should "retrieve a list of students by ids" in {
    Await.result(studentLayer.selectByIds(1, 2, 3), 3.seconds) shouldBe Right(Seq(
      Student(1, "John Doe", "john@doe.com"),
      Student(2, "Jane Doe", "jane@doe.com")
    ))
  }
}
