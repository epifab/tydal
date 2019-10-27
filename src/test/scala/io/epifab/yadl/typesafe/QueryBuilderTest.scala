package io.epifab.yadl.typesafe

import java.time.{Instant, LocalDate}

import io.epifab.yadl.typesafe.Schema.{Address, Interest, Students}
import io.epifab.yadl.typesafe.SelectQueries._
import io.epifab.yadl.typesafe.fields.Placeholder
import org.scalatest.{FlatSpec, Matchers}
import shapeless.HNil

class QueryBuilderTest extends FlatSpec with Matchers {
  "The QueryBuilder" should "build the simplest select query" in {
    Select.query shouldBe Query("SELECT 1", HNil, HNil)
  }

  it should "build a simple select query" in {
    studentsQuery("ms").select.query shouldBe Query(
      "SELECT" +
        " e.student_id AS student_id," +
        " max(e.score) AS max_score," +
        " min(e.course_id) AS course_id" +
        " FROM exams AS e" +
        " WHERE e.registration_timestamp < ?::timestamp" +
        " GROUP BY e.student_id",
      Placeholder[Instant, "min_date"] :: HNil,
      studentsQuery("ms").select.fields
    )
  }

  it should "build a select query with join" in {
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

  it should "build a select query with subqueries" in {
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
      Placeholder[Int, "min_date"] :: Placeholder[Int, "student_id"] :: HNil,
      studentsQuery.fields
    )
  }

  it should "build an insert query" in {
    Insert.into(Students).query shouldBe Query(
      "INSERT INTO students (id, name, email, date_of_birth, address, interests)" +
        " VALUES (?::int, ?::varchar, ?::varchar, ?::date, ?::json, ?::interest[])",
      Placeholder[Int, "id"] ::
        Placeholder[String, "name"] ::
        Placeholder[Option[String], "email"] ::
        Placeholder[LocalDate, "date_of_birth"] ::
        Placeholder[Option[Address], "address"] ::
        Placeholder[Seq[Interest], "interests"] ::
        HNil,
      HNil
    )
  }
}
