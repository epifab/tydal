package io.epifab.yadl.postgres

import io.epifab.yadl.domain._
import io.circe.generic.auto._
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class PostgresQueryBuildersTest extends FlatSpec {
  import io.epifab.yadl.examples.Schema._
  import io.epifab.yadl.implicits._
  import io.epifab.yadl.postgres.PostgresQueryBuilder._

  val students = new StudentsTable("s")

  "PostgresQuery" should "evaluate a the simplest query" in {
    val query = Select.from(students)
    select(query) shouldBe Query(
      "SELECT 1" +
        " FROM students AS s" +
        " WHERE 1 = 1")
  }

  it should "evaluate a query with 1 field" in {
    val query = Select
      .from(students)
      .take(students.name)

    select(query) shouldBe Query(
      "SELECT s.name AS s__name" +
        " FROM students AS s" +
        " WHERE 1 = 1")
  }

  it should "evaluate a query with 2 fields" in {
    val query = Select
      .from(students)
      .take(students.name, students.email)

    select(query) shouldBe Query(
      "SELECT s.name AS s__name, s.email AS s__email" +
        " FROM students AS s" +
        " WHERE 1 = 1")
  }

  it should "evaluate a query with a filter" in {
    val query = Select
      .from(students)
      .take(students.name)
      .where(students.name === Value("Fabio"))

    select(query) shouldBe Query(
      "SELECT s.name AS s__name" +
        " FROM students AS s" +
        " WHERE s.name = ?", Seq(Value("Fabio")))
  }

  it should "evaluate non trivial filters" in {
    val query =
      Select
        .from(students)
        .where(
          (students.name === Value("Fabio"))
            and (students.email like Value("epifab@%"))
            or (students.id in Value(Seq(1, 2, 6))))

    select(query) shouldBe Query(
      "SELECT 1" +
        " FROM students AS s" +
        " WHERE (s.name = ? AND s.email LIKE ? OR s.id = ANY(?))",
      Seq(
        Value("Fabio"),
        Value("epifab@%"),
        Value(Seq(1, 2, 6))
      )
    )
  }

  it should "evaluate non trivial filters respecting precedence" in {
    Select.from(students)

    val query =
      Select
        .from(students)
        .where(
          students.name === Value("Fabio")
            and
            (students.email like Value("epifab@%") or (students.id in Value(Seq(1, 2, 6))))
        )

    select(query) shouldBe Query(
      "SELECT 1" +
        " FROM students AS s" +
        " WHERE s.name = ? AND (s.email LIKE ? OR s.id = ANY(?))",
      Seq(Value("Fabio"), Value("epifab@%"), Value(Seq(1, 2, 6)))
    )
  }

  it should "evaluate left and inner joins" in {
    val query =
      Select
        .from(students)
        .leftJoin(students.exams, students.exams.studentId === students.id)
        .innerJoin(students.exams.course)
        .take(students.name, students.exams.rate, students.exams.course.name)

    select(query) shouldBe Query(
      "SELECT s.name AS s__name," +
        " s__exams.rate AS s__exams__rate," +
        " s__exams__course.name AS s__exams__course__name" +
        " FROM students AS s" +
        " LEFT JOIN exams AS s__exams ON s__exams.student_id = s.id" +
        " INNER JOIN courses AS s__exams__course ON s__exams__course.id = s__exams.course_id" +
        " WHERE 1 = 1"
    )
  }

  it should "evaluate sort" in {
    val query = Select
      .from(students)
      .take(students.name)
      .sortBy(students.email.asc, students.id.desc)

    select(query) shouldBe Query(
      "SELECT s.name AS s__name" +
        " FROM students AS s" +
        " WHERE 1 = 1" +
        " ORDER BY s.email ASC, s.id DESC")
  }

  it should "evaluate limit" in {
    val query = Select
      .from(students)
      .take(students.name)
      .inRange(1, 4)

    select(query) shouldBe Query(
      "SELECT s.name AS s__name" +
        " FROM students AS s" +
        " WHERE 1 = 1" +
        " OFFSET 1 LIMIT 4")
  }

  it should "evaluate an insert query" in {
    val query =
      Insert
        .into(students)
        .set(
          students.id -> 123,
          students.name -> "John",
          students.email -> Some("john@doe.com"),
          students.address -> Some(Json(Address("n1900", "123 Fake St.", None)))
        )

    insert(query) shouldBe Query(
      "INSERT INTO students" +
        " (id, name, email, address)" +
        " VALUES (?, ?, ?, ?)",
      Seq(
        Value(123),
        Value("John"),
        Value(Option("john@doe.com")),
        Value(Option(Json(Address("n1900", "123 Fake St.", None))))
      )
    )
  }

  it should "evaluate an update query" in {
    val query =
      Update(students)
        .set(
          students.name -> "Jane",
          students.email -> Some("jane@doe.com")
        )
        .where(students.id === Value(2))

    update(query) shouldBe Query(
      "UPDATE students AS s" +
        " SET name = ?, email = ?" +
        " WHERE s.id = ?",
      Seq(FieldValue(students.name, "Jane"), FieldValue(students.email, Some("jane@doe.com")), Value(2))
    )
  }

  it should "evaluate a delete query" in {
    val query =
      Delete(students)
        .where(students.id === Value(2))

    delete(query) shouldBe Query(
      "DELETE FROM students AS s" +
        " WHERE s.id = ?",
      Seq(Value(2))
    )
  }
}
