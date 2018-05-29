import io.epifab.dal.domain._
import io.epifab.dal.Query
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class PostgresQueryBuildersTest extends FlatSpec {
  import io.epifab.dal.Implicits._
  import io.epifab.dal.PostgresQueryBuilders._
  import Schema._

  "PostgresQuery" should "evaluate a the simplest query" in {
    val query = Select(students)
    select(query) shouldBe Query(
      "SELECT 1" +
        " FROM hd_students AS s" +
        " WHERE 1 = 1")
  }

  it should "evaluate a query with 1 field" in {
    val query = Select
      .from(students)
      .take(students.name)

    select(query) shouldBe Query(
      "SELECT s.name AS s__name" +
        " FROM hd_students AS s" +
        " WHERE 1 = 1")
  }

  it should "evaluate a query with 2 fields" in {
    val query = Select
      .from(students)
      .take(students.name, students.email)

    select(query) shouldBe Query(
      "SELECT s.name AS s__name, s.email AS s__email" +
        " FROM hd_students AS s" +
        " WHERE 1 = 1")
  }

  it should "evaluate a query with a filter" in {
    val query = Select
      .from(students)
      .take(students.name)
      .where(students.name === "Fabio")

    select(query) shouldBe Query(
      "SELECT s.name AS s__name" +
        " FROM hd_students AS s" +
        " WHERE s.name = ?", Seq("Fabio"))
  }

  it should "evaluate non trivial filters" in {
    val query =
      Select
        .from(students)
        .where(
          (students.name === "Fabio")
            and (students.email like "epifab@%")
            or (students.id in List(1, 2, 6)))

    select(query) shouldBe Query(
      "SELECT 1" +
        " FROM hd_students AS s" +
        " WHERE (s.name = ? AND s.email LIKE ? OR s.id IN (?, ?, ?))",
      Seq("Fabio", "epifab@%", 1, 2, 6)
    )
  }

  it should "evaluate non trivial filters respecting precedence" in {
    Select(students)

    val query =
      Select
        .from(students)
        .where(
          students.name === "Fabio"
            and
            (students.email like "epifab@%" or (students.id in List(1, 2, 6)))
        )

    select(query) shouldBe Query(
      "SELECT 1" +
        " FROM hd_students AS s" +
        " WHERE s.name = ? AND (s.email LIKE ? OR s.id IN (?, ?, ?))",
      Seq("Fabio", "epifab@%", 1, 2, 6)
    )
  }

  it should "evaluate left and inner joins" in {
    val query =
      Select
        .from(students)
        .leftJoin(students.exams, students.exams.studentId === students.id)
        .innerJoin(students.exams.course, students.exams.course.id === students.exams.courseId)
        .take(students.name, students.exams.rate, students.exams.course.name)

    select(query) shouldBe Query(
      "SELECT s.name AS s__name, e.rate AS e__rate, c.name AS c__name" +
        " FROM hd_students AS s" +
        " LEFT JOIN hd_exams AS e ON e.student_id = s.id" +
        " INNER JOIN hd_courses AS c ON c.id = e.course_id" +
        " WHERE 1 = 1"
    )
  }

  it should "evaluate an insert query" in {
    val query =
      Insert
        .into(students)
        .set(
          students.name -> "John",
          students.email -> "john@doe.com"
        )

    insert(query) shouldBe Query(
      "INSERT INTO hd_students" +
        " (name, email)" +
        " VALUES (?, ?)",
      Seq("John", "john@doe.com")
    )
  }
}
