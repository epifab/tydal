package io.epifab.yadl.typesafe

import java.time.Instant

import io.epifab.yadl.typesafe.SelectQueries._
import io.epifab.yadl.typesafe.fields.Placeholder
import org.scalatest.{FlatSpec, Matchers}
import shapeless.HNil

class QueryBuilderTest extends FlatSpec with Matchers {
  "The QueryBuilder" should "build the simplest query" in {
    Select.query shouldBe Query("SELECT 1", HNil, HNil)
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
      Placeholder["min_date", Instant] :: HNil,
      studentsQuery("ms").select.fields
    )
  }

  it should "build a query with join" in {
    examsWithCourseQuery.query shouldBe Query(
      "SELECT" +
        " c.name AS cname," +
        " e.score AS score" +
        " FROM exams AS e" +
        " INNER JOIN courses AS c ON c.id = e.course_id",
      HNil,
      examsWithCourseQuery.fields
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
      Placeholder["min_date", Int] :: Placeholder["student_id", Int] :: HNil,
      studentsQuery.fields
    )
  }
}
