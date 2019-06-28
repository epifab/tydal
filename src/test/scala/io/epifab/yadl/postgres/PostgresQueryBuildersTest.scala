package io.epifab.yadl.postgres

import java.time.LocalDate

import io.epifab.yadl.domain._
import io.epifab.yadl.examples.Address
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import shapeless.HNil

class PostgresQueryBuildersTest extends FlatSpec {
  import io.epifab.yadl.examples.Schema._
  import io.epifab.yadl.implicits._
  import io.epifab.yadl.postgres.PostgresQueryBuilder.build
  import io.epifab.yadl.examples.Adapters._

  val students = new StudentsTable
  val exams = new ExamsTable

  class ExamsSubQuery extends SubQuery[Int, Int] {
    val studentId: Term[Int] = term(exams.studentId)

    override def select: Select[Int] =
      Select.from(exams)
        .take(Terms(exams.studentId))

    override def `*`: Terms[Int] = Terms(studentId)
  }

  "PostgresQuery" should "evaluate a the simplest query" in {
    val query = Select.from(students)
    build(query) shouldBe Query(
      "SELECT 1" +
        " FROM students AS ds1" +
        " WHERE 1 = 1")
  }

  it should "evaluate a query with 1 column" in {
    val query = Select
      .from(students)
      .take(Terms(students.name))

    build(query) shouldBe Query(
      "SELECT ds1.name AS ds1__name" +
        " FROM students AS ds1" +
        " WHERE 1 = 1")
  }

  it should "evaluate a query with 2 columns" in {
    val query = Select
      .from(students)
      .take(Terms(students.name :: students.email :: HNil))

    build(query) shouldBe Query(
      "SELECT ds1.name AS ds1__name, ds1.email AS ds1__email" +
        " FROM students AS ds1" +
        " WHERE 1 = 1")
  }

  it should "evaluate a query with a filter" in {
    val query = Select
      .from(students)
      .take(Terms(students.name))
      .where(students.name === Value("Fabio"))

    build(query) shouldBe Query(
      "SELECT ds1.name AS ds1__name" +
        " FROM students AS ds1" +
        " WHERE ds1.name = ?", Seq(Value("Fabio")))
  }

  it should "evaluate non trivial filters" in {
    val query =
      Select
        .from(students)
        .where(
          (students.name === Value("Fabio"))
            and (students.email like Value("epifab@%"))
            or (students.id in Value(Seq(1, 2, 6))))

    build(query) shouldBe Query(
      "SELECT 1" +
        " FROM students AS ds1" +
        " WHERE (ds1.name = ? AND ds1.email LIKE ? OR ds1.id = ANY(?))",
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

    build(query) shouldBe Query(
      "SELECT 1" +
        " FROM students AS ds1" +
        " WHERE ds1.name = ? AND (ds1.email LIKE ? OR ds1.id = ANY(?))",
      Seq(Value("Fabio"), Value("epifab@%"), Value(Seq(1, 2, 6)))
    )
  }

  it should "evaluate left and inner joins" in {
    val query =
      Select
        .from(students)
        .leftJoin(students.exams.on(_.studentId === students.id))
        .innerJoin(students.exams.course)
        .take(Terms(
          students.name ::
          students.exams.score ::
          students.exams.course.name ::
          HNil
        ))

    build(query) shouldBe Query(
      "SELECT ds1.name AS ds1__name," +
        " ds2.score AS ds2__score," +
        " ds3.name AS ds3__name" +
        " FROM students AS ds1" +
        " LEFT JOIN exams AS ds2 ON ds2.student_id = ds1.id" +
        " INNER JOIN courses AS ds3 ON ds3.id = ds2.course_id" +
        " WHERE 1 = 1"
    )
  }

  it should "evaluate sort" in {
    val query = Select
      .from(students)
      .take(Terms(students.name))
      .sortBy(Asc(students.email), Desc(students.id))

    build(query) shouldBe Query(
      "SELECT ds1.name AS ds1__name" +
        " FROM students AS ds1" +
        " WHERE 1 = 1" +
        " ORDER BY ds1.email ASC, ds1.id DESC")
  }

  it should "evaluate limit" in {
    val query = Select
      .from(students)
      .take(Terms(students.name))
      .inRange(1, 4)

    build(query) shouldBe Query(
      "SELECT ds1.name AS ds1__name" +
        " FROM students AS ds1" +
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
          students.address -> Some(Address("n1900", "123 Fake St.", None)),
          students.dateOfBirth -> LocalDate.of(1985, 11, 15)
        )

    build(query) shouldBe Query(
      "INSERT INTO students" +
        " (id, name, email, address, date_of_birth)" +
        " VALUES (?, ?, ?, cast(? as json), cast(? as date))",
      Seq(
        Value(123),
        Value("John"),
        Value(Option("john@doe.com")),
        Value(Option(Address("n1900", "123 Fake St.", None))),
        Value(LocalDate.of(1985, 11, 15))
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

    build(query) shouldBe Query(
      "UPDATE students AS ds1" +
        " SET name = ?, email = ?" +
        " WHERE ds1.id = ?",
      Seq(Value("Jane"), Value(Option("jane@doe.com")), Value(2))
    )
  }

  it should "evaluate a delete query" in {
    val query =
      Delete(students)
        .where(students.id === Value(2))

    build(query) shouldBe Query(
      "DELETE FROM students AS ds1" +
        " WHERE ds1.id = ?",
      Seq(Value(2))
    )
  }

  it should "aggregate columns" in {
    val query =
      Select
        .from(exams)
        .take(Terms(
          exams.studentId ::
          Avg(exams.score) ::
          Count(exams.courseId) ::
          Max(exams.score) ::
          Min(exams.score) ::
          HNil
        ))
        .groupBy(exams.studentId)

    build(query) shouldBe Query(
      "SELECT" +
        " ds1.student_id AS ds1__student_id," +
        " avg(ds1.score) AS avg_ds1__score," +
        " count(ds1.course_id) AS count_ds1__course_id," +
        " max(ds1.score) AS max_ds1__score," +
        " min(ds1.score) AS min_ds1__score" +
        " FROM exams AS ds1" +
        " WHERE 1 = 1" +
        " GROUP BY ds1.student_id"
    )
  }

  it should "evaluate a distinct" in {
    val query =
      Select
        .from(students)
        .take(Terms(Distinct(students.name)))

    build(query) shouldBe Query(
      "SELECT DISTINCT ds1.name AS ds1__name" +
        " FROM students AS ds1" +
        " WHERE 1 = 1"
    )
  }

  it should "evaluate a count distinct" in {
    val query =
      Select
        .from(students)
        .take(Terms(Count(Distinct(students.name))))

    build(query) shouldBe Query(
      "SELECT count(DISTINCT ds1.name) AS count_ds1__name" +
        " FROM students AS ds1" +
        " WHERE 1 = 1"
    )
  }

  it should "evaluate a simple subquery" in {
    val exams = new ExamsSubQuery

    val query = Select
      .from(exams)
      .take(exams.*)

    build(query) shouldBe Query(
      "SELECT" +
        " ds1.ds2__student_id AS ds1__ds2__student_id" +
        " FROM (SELECT ds2.student_id AS ds2__student_id FROM exams AS ds2 WHERE 1 = 1) AS ds1" +
        " WHERE 1 = 1"
    )
  }

  it should "evaluate a subquery in a join" in {
    val exams = new ExamsSubQuery

    val query = Select
      .from(students)
      .innerJoin(exams.on(_.studentId === students.id))
      .take(Terms(students.id :: exams.studentId :: HNil))

    build(query) shouldBe Query(
      "SELECT" +
        " ds1.id AS ds1__id," +
        " ds2.ds3__student_id AS ds2__ds3__student_id" +
        " FROM students AS ds1" +
        " INNER JOIN (SELECT ds3.student_id AS ds3__student_id FROM exams AS ds3 WHERE 1 = 1) AS ds2" +
        " ON ds2.ds3__student_id = ds1.id" +
        " WHERE 1 = 1"
    )
  }

  it should "integrate nicely with PostGis" in {
    val place = new PlaceTable

    val distanceToMe: Term[Option[Double]] =
      Distance(
        place.coordinates.castTo[Option[Geography]],
        MakePoint(Value(0.0), Value(0.0)).castTo[Option[Geography]]
      )

    val query = Select
      .from(place)
      .take(Terms(distanceToMe))
      .where(distanceToMe <= Value(4.0))
      .sortBy(Asc(distanceToMe))

    build(query) shouldBe Query(
      "SELECT" +
        " ST_Distance(ds1.coordinates::geography, ST_MakePoint(?, ?)::geography) AS ST_Distance_ds1__coordinates__as_geography_ST_MakePoint_ds2_ds2__as_geography" +
        " FROM place AS ds1" +
        " WHERE ST_Distance(ds1.coordinates, ST_MakePoint(?, ?)) <= ?" +
        " ORDER BY ST_Distance(ds1.coordinates, ST_MakePoint(?, ?)) ASC",
      List(Value(0.0), Value(0.0), Value(0.0), Value(0.0), Value(4.0), Value(0.0), Value(0.0)))
  }
}
