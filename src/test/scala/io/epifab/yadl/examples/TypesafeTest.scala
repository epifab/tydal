package io.epifab.yadl.examples

import java.time.{Instant, LocalDate}

import io.epifab.yadl.examples.TypesafeSchema.{Courses, Exams, Students}
import io.epifab.yadl.typesafe._
import io.epifab.yadl.typesafe.fields._
import org.scalatest.{FlatSpec, Matchers}
import shapeless.{::, HNil}

object SelectsQueries {
  import Implicits._

  val studentsQuery = {
    val studentId = Placeholder["student_id", Int]

    val maxScoreSubQuery =
      Select
        .from(Exams.as["e"])
        .groupBy(_["e", "student_id"].get :: HNil)
        .take($ =>
          $["e", "student_id"].get ::
            Max($["e", "score"].get).as["max_score"] ::
            Min($["e", "course_id"].get).as["course_id"] ::
            HNil
        )
        .subQuery

    Select
      .from(Students.as["s"])
      .join($ => maxScoreSubQuery.as["ms"]
        .on(_["student_id"].get === $["s", "id"].get))
      .join($ => Courses.as["cc"].on(_["id"].get === $["ms", "course_id"].get))
      .take($ =>
        $["s", "id"].get.as["sid"] ::
        $["s", "name"].get.as["sname"] ::
        $["ms", "max_score"].get.as["score"] ::
        $["cc", "name"].get.as["cname"] ::
        HNil
      )
      .where($ => ($["s", "id"].get === studentId) and
        ($["s", "id"].get !== studentId))
  }

  val examsWithCourseQuery =
    Select
      .from(Exams.as["e"])
      .join($ => Courses.as["c"].on(_["id"].get === $["e", "course_id"].get))
      .take($ =>
        $["c", "name"].get.as["cname"] ::
        $["e", "score"].get.as["score"] ::
        HNil)
}

class TypesafeTest extends FlatSpec with Matchers {
  import SelectsQueries._

  "The TagMap typeclass" should "bind fields to their alias" in {
    val fields: Map[String, Field[Any]] =
      Tagged(studentsQuery.fields)

    fields.keys.toSet shouldBe Set("sid", "sname", "score", "cname")
  }

  "The source finder" should "get any source" in {
    examsWithCourseQuery["cname"].get shouldBe a[Field[String] with Tag["cname"]]
    examsWithCourseQuery["score"].get shouldBe a[Field[Int] with Tag["score"]]
    examsWithCourseQuery["e"].get shouldBe a[Table["exams", _] with Tag["e"]]
    examsWithCourseQuery["c"].get shouldBe a[Table["courses", _] with Tag["c"]]
  }

  "The QueryBuilder" should "build the simplest query" in {
    Select.query shouldBe Query("SELECT 1")
  }

  it should "build a simple query" in {
    studentsQuery["ms"].get.select.query shouldBe Query(
      "SELECT" +
        " e.student_id AS student_id," +
        " max(e.score) AS max_score," +
        " min(e.course_id) AS course_id" +
        " FROM exams AS e" +
        " GROUP BY e.student_id"
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
        " WHERE s.id = ? AND s.id != ?",
      Seq(
        Placeholder["student_id", Int],
        Placeholder["student_id", Int]
      )
    )

    studentsQuery.placeholders shouldBe Placeholder["student_id", Int] :: Placeholder["student_id", Int] :: HNil
  }
}
