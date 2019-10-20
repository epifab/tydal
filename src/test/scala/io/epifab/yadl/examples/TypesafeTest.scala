package io.epifab.yadl.examples

import java.time.Instant

import io.epifab.yadl.{PostgresConfig, PostgresConnection}
import io.epifab.yadl.examples.TypesafeSchema.Codecs._
import io.epifab.yadl.examples.TypesafeSchema._
import io.epifab.yadl.typesafe._
import io.epifab.yadl.typesafe.fields._
import org.scalatest.{FlatSpec, Matchers}
import shapeless.HNil

object SelectsQueries {
  import Implicits._

  val studentsQuery = {
    val studentId = Placeholder["student_id", Int]

    val maxScoreSubQuery =
      Select
        .from(Exams as "e")
        .groupBy(_("e", "student_id") :: HNil)
        .where(_("e", "registration_timestamp") < Placeholder["min_date", Instant])
        .take($ =>
          $("e", "student_id") ::
            Max($("e", "score")).as["max_score"] ::
            Min($("e", "course_id")).as["course_id"] ::
            HNil
        )
        .subQuery

    Select
      .from(Students as "s")
      .join($ => maxScoreSubQuery.as["ms"]
        .on(_("student_id") === $("s", "id")))
      .join($ => (Courses as "cc").on(_("id") === $("ms", "course_id")))
      .take($ =>
        $("s", "id").as["sid"] ::
        $("s", "name").as["sname"] ::
        $("ms", "max_score").as["score"] ::
        Nullable($("cc", "name")).as["cname"] ::
        HNil
      )
      .where(_("s", "id") === Placeholder["student_id", Int])
  }

  val examsWithCourseQuery =
    Select
      .from(Exams as "e")
      .join($ => (Courses as "c") on (_("id") === $("e", "course_id")))
      .take($ =>
        $("c", "name").as["cname"] ::
        $("e", "score").as["score"] ::
        HNil)
}

class TypesafeTest extends FlatSpec with Matchers {
  import SelectsQueries._

  "The TagMap typeclass" should "bind fields to their alias" in {
    val fields: Map[String, Field[Any]] =
      TagMap(studentsQuery.fields)

    fields.keys.toSet shouldBe Set("sid", "sname", "score", "cname")
  }

  "The source finder" should "get any source" in {
    examsWithCourseQuery("cname") shouldBe a[Field[String] with Tag["cname"]]
    examsWithCourseQuery("score") shouldBe a[Field[Int] with Tag["score"]]
    examsWithCourseQuery("e") shouldBe a[Table["exams", _] with Tag["e"]]
    examsWithCourseQuery("c") shouldBe a[Table["courses", _] with Tag["c"]]
  }

  "The QueryBuilder" should "build the simplest query" in {
    Select.query shouldBe Query("SELECT 1")
  }

  it should "build a simple query" in {
    studentsQuery("ms").select.query shouldBe Query(
      "SELECT" +
        " e.student_id AS student_id," +
        " max(e.score) AS max_score," +
        " min(e.course_id) AS course_id" +
        " FROM exams AS e" +
        " WHERE e.registration_timestamp < ?::timestamp" +
        " GROUP BY e.student_id",
      Seq(Placeholder["min_date", Instant])
    )
  }

  it should "build a query with join" in {
    examsWithCourseQuery.query shouldBe Query(
      "SELECT" +
        " c.name AS cname," +
        " e.score AS score" +
        " FROM exams AS e" +
        " INNER JOIN courses AS c ON c.id = e.course_id"
    )
  }

  it should "build a query with subqueries" in {
    val subQuery =
      "SELECT e.student_id AS student_id," +
        " max(e.score) AS max_score," +
        " min(e.course_id) AS course_id" +
        " FROM exams AS e" +
        " WHERE e.registration_timestamp < ?::timestamp" +
        " GROUP BY e.student_id"

    studentsQuery.query shouldBe Query(
      "SELECT" +
        " s.id AS sid," +
        " s.name AS sname," +
        " ms.max_score AS score," +
        " cc.name AS cname" +
        " FROM students AS s" +
        " INNER JOIN (" + subQuery + ") AS ms ON ms.student_id = s.id" +
        " INNER JOIN courses AS cc ON cc.id = ms.course_id" +
        " WHERE s.id = ?::int",
      Seq(
        Placeholder["min_date", Int],
        Placeholder["student_id", Int]
      )
    )
  }

  it should "run a query successfully" in {
    case class Student(id: Int, name: String, bestScore: Option[Int], bestCourse: Option[String])

    val students: Either[DecoderError, Seq[Student]] =
      studentsQuery.compile
        .mapTo[Student]
        .runSync(
          PostgresConnection(PostgresConfig.fromEnv()),
          Value("min_date", Instant.now) :: Value("student_id", 3) :: HNil
        )

    students shouldBe Symbol("Right")
  }
}
