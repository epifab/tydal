package io.epifab.tydal.typesafe

import io.epifab.tydal.examples.Schema._
import io.epifab.tydal.examples.StudentsRepo
import io.epifab.tydal.queries.{Delete, Insert, Select, Update}
import io.epifab.tydal.typesafe.SelectQueries._
import org.scalatest.{FlatSpec, Matchers}

class QueryBuilderTest extends FlatSpec with Matchers {
//  "The QueryBuilder" should "build the simplest select query" in {
//    Select.compile.query shouldBe "SELECT 1"
//  }

  it should "build a query with offset and limit" in {
    queryWithRange.compile.query shouldBe (
      "SELECT" +
        " e.student_id AS sid" +
        " FROM exams AS e" +
        " OFFSET ?::bigint" +
        " LIMIT ?::int"
    )
  }

  it should "build a simple select query" in {
    maxScoreSubQuery.compile.query shouldBe (
      "SELECT" +
        " e.student_id AS student_id," +
        " max(e.score) AS max_score," +
        " min(e.course_id) AS course_id" +
        " FROM exams AS e" +
        " WHERE e.exam_timestamp < ?::timestamp" +
        " GROUP BY e.student_id"
      )
  }

  it should "build a select query with join" in {
    examsWithCourseQuery.compile.query shouldBe (
      "SELECT" +
        " c.name AS cname," +
        " e.score AS score" +
        " FROM exams AS e" +
        " INNER JOIN courses AS c ON c.id = e.course_id"
    )
  }

  it should "build a select query with subqueries" in {
    val subQuery =
      "SELECT e.student_id AS student_id," +
        " max(e.score) AS max_score," +
        " min(e.course_id) AS course_id" +
        " FROM exams AS e" +
        " WHERE e.exam_timestamp < ?::timestamp" +
        " GROUP BY e.student_id"

    studentsQuery.compile.query shouldBe (
      "SELECT" +
        " s.id AS sid," +
        " s.name AS sname," +
        " ms.max_score AS score," +
        " cc.name AS cname" +
        " FROM students AS s" +
        " INNER JOIN (" + subQuery + ") AS ms ON ms.student_id = s.id" +
        " INNER JOIN courses AS cc ON cc.id = ms.course_id" +
        " WHERE s.id = ?::int"
    )
  }

  it should "build an insert query" in {
    Insert.into(Students).compile.query shouldBe (
      "INSERT INTO students (id, name, email, date_of_birth, address, interests)" +
        " VALUES (?::int, ?::varchar, ?::varchar, ?::date, ?::json, ?::interest[])",
    )
  }

  it should "build an update query without filter" in {
    Update(Students).fields(s => (s("name"), s("email"))).compile.query shouldBe (
      "UPDATE students SET" +
        " name = ?::varchar," +
        " email = ?::varchar"
      )
  }

  it should "build an update query" in {
    updateStudentQuery.compile.query shouldBe (
      "UPDATE students SET" +
        " name = ?::varchar," +
        " email = ?::varchar" +
        " WHERE students.id = ?::int"
    )
  }

  it should "build a delete query without filter" in {
    Delete.from(Students).compile.query shouldBe
      "DELETE FROM students"
  }

  it should "build a delete query" in {
    deleteStudentQuery.compile.query shouldBe (
      "DELETE FROM students WHERE" +
        " students.id = ?::int"
    )
  }

  it should "build a query with a IN subquery clause" in {
    StudentsRepo.studentsWithMinScore.query shouldBe (
      "SELECT" +
        " s.id AS id," +
        " s.name AS name," +
        " s.email AS email," +
        " s.date_of_birth AS date_of_birth," +
        " s.address AS address," +
        " s.interests AS interests" +
        " FROM students AS s" +
        " WHERE s.id IN (SELECT e.student_id AS student_id FROM exams AS e WHERE e.score >= ?::int)"
    )
  }
}
