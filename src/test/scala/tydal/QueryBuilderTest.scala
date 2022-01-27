package tydal

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tydal.SelectQueries._
import tydal.queries._
import tydal.university.Schema._
import tydal.university.StudentsRepo

class QueryBuilderTest extends AnyFlatSpec with Matchers {
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

  it should "build a sort by clause" in {
    maxScoreSubQuery.sortBy($ => Descending($("max_score")) -> Ascending($("student_id"))).compile.query shouldBe (
      "SELECT" +
        " e.student_id AS student_id," +
        " max(e.score) AS max_score," +
        " min(e.course_id) AS course_id" +
        " FROM exams AS e" +
        " WHERE e.exam_timestamp < ?::timestamp" +
        " GROUP BY e.student_id" +
        " ORDER BY max(e.score) DESC, e.student_id ASC"
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
        " WHERE s.id = ?::uuid"
    )
  }

  it should "build an insert query" in {
    Insert.into(Students).compile.query shouldBe (
      "INSERT INTO students (id, name, email, date_of_birth, address, interests)" +
        " VALUES (?::uuid, ?::varchar, ?::varchar, ?::date, ?::json, ?::interest[])",
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
        " WHERE students.id = ?::uuid"
    )
  }

  it should "build a delete query without filter" in {
    Delete.from(Students).compile.query shouldBe
      "DELETE FROM students"
  }

  it should "build a delete query" in {
    deleteStudentQuery.compile.query shouldBe (
      "DELETE FROM students WHERE" +
        " students.id = ?::uuid"
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

  it should "build an update query (on conflict do nothing)" in {
    Insert.into(Students)
      .onConflict(s => Tuple1(s("email")))
      .doNothing.compile.query shouldBe (
      "INSERT INTO students (id, name, email, date_of_birth, address, interests)" +
        " VALUES (?::uuid, ?::varchar, ?::varchar, ?::date, ?::json, ?::interest[])" +
        " ON CONFLICT (email) DO NOTHING"
    )
  }

  it should "build an update query (on conflict do update)" in {
    Insert.into(Students)
      .onConflict(s => Tuple1(s("email")))
      .doUpdate(s => (s("date_of_birth"), s("address"), s("interests")))
      .compile.query shouldBe (
      "INSERT INTO students (id, name, email, date_of_birth, address, interests)" +
        " VALUES (?::uuid, ?::varchar, ?::varchar, ?::date, ?::json, ?::interest[])" +
        " ON CONFLICT (email) DO UPDATE SET date_of_birth = ?::date, address = ?::json, interests = ?::interest[]"
    )
  }
}
