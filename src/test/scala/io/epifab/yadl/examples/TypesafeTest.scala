package io.epifab.yadl.examples

import io.epifab.yadl.examples.TypesafeSchema.{Courses, Exams, Students}
import io.epifab.yadl.typesafe._
import io.epifab.yadl.typesafe.fields._
import org.scalatest.{FlatSpec, Matchers}
import shapeless.{::, HNil}

object SelectsQueries {
  import Implicits._

  def maxScoreSubQuery[E <: String](implicit eAlias: ValueOf[E]) =
    Select
      .from(Exams.as[E])
      .groupBy(_[E, "student_id"].get :: HNil)
      .take($ =>
        $[E, "student_id"].get ::
          Max($[E, "score"].get).as["max_score"] ::
          Min($[E, "course_id"].get).as["course_id"] ::
          HNil)
      .subQuery

  val studentsQuery =
    Select
      .from(Students.as["s"])
      .join($ => maxScoreSubQuery["e"].as["ms"]
        .on(_["student_id"].get === $["s", "id"].get))
      .join($ => Courses.as["cc"].on(_["id"].get === $["ms", "course_id"].get))
      .take($ =>
        $["s", "id"].get.as["sid"] ::
          $["s", "name"].get.as["sname"] ::
          $["ms", "max_score"].get.as["score"] ::
          $["cc", "name"].get.as["cname"] ::
          HNil
      )
      .where($ => $["s", "id"].get === Placeholder["student_id", Int])

  val examsWithCourse =
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
      TagMap(studentsQuery.fields)

    fields.keys.toSet shouldBe Set("sid", "sname", "score", "cname")
  }

  "The source finder" should "get any source" in {
    examsWithCourse["cname"].get shouldBe a[Field[String] with Tag["cname"]]
    examsWithCourse["score"].get shouldBe a[Field[Int] with Tag["score"]]
    examsWithCourse["e"].get shouldBe a[Table["exams", _] with Tag["e"]]
    examsWithCourse["c"].get shouldBe a[Table["courses", _] with Tag["c"]]
  }

  "The QueryBuilder" should "build a simple query" in {
    QueryBuilder(studentsQuery["ms"].get.select) shouldBe (
      "SELECT" +
        " e.student_id AS student_id," +
        " max(e.score) AS max_score," +
        " min(e.course_id) AS course_id" +
        " FROM exams AS e" +
        " GROUP BY e.student_id"
    )
  }

  it should "build a query with join" in {
    QueryBuilder(examsWithCourse) shouldBe (
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

    QueryBuilder(studentsQuery) shouldBe (
      "SELECT" +
        " s.id AS sid," +
        " s.name AS sname," +
        " ms.max_score AS score," +
        " cc.name AS cname" +
        " FROM students AS s" +
        " INNER JOIN (" + subQuery + ") AS ms ON ms.student_id = s.id" +
        " INNER JOIN courses AS cc ON cc.id = ms.course_id" +
        " WHERE s.id = :student_id"
    )
  }
}
