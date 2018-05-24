import domain._
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class PostgresQueryBuildersTest extends FlatSpec {
  import PostgresQueryBuilders._
  import FieldExtractor._

  val students: Table = Table("students", "s")
  val `student.id`: Field[Int] = students("id")
  val `student.name`: Field[String] = students("name")
  val `student.email`: Field[String] = students("email")

  val exams: Table = Table("exams", "e")
  val `exams.student_id`: Field[Int] = exams("student_id")
  val `exams.course_id`: Field[Int] = exams("course_id")
  val `exams.rate`: Field[Int] = exams("rate")

  val course: Table = Table("courses", "c")
  val `course.id`: Field[Int] = course("id")
  val `course_name`: Field[String] = course("name")

  "PostgresQuery" should "evaluate a the simplest query" in {
    val query = SelectQuery(students)
    select(query) shouldBe Query(
      "SELECT 1" +
        " FROM students AS s" +
        " WHERE 1 = 1")
  }

  it should "evaluate a query with 1 field" in {
    val query = SelectQuery
      .from(students)
      .take(`student.name`)

    select(query) shouldBe Query(
      "SELECT s.name AS s__name" +
        " FROM students AS s" +
        " WHERE 1 = 1")
  }

  it should "evaluate a query with 2 fields" in {
    val query = SelectQuery
      .from(students)
      .take(`student.name`, `student.email`)

    select(query) shouldBe Query(
      "SELECT s.name AS s__name, s.email AS s__email" +
        " FROM students AS s" +
        " WHERE 1 = 1")
  }

  it should "evaluate a query with a filter" in {
    import Filter._

    val query = SelectQuery
      .from(students)
      .take(`student.name`)
      .where(`student.name` === "Fabio")

    select(query) shouldBe Query(
      "SELECT s.name AS s__name" +
        " FROM students AS s" +
        " WHERE s.name = ?", Seq("Fabio"))
  }

  it should "evaluate non trivial filters" in {
    import Filter._

    val query =
      SelectQuery
        .from(students)
        .where(
          (`student.name` === "Fabio")
            and (`student.email` like "epifab@%")
            or (`student.id` in List(1, 2, 6)))

    select(query) shouldBe Query(
      "SELECT 1" +
        " FROM students AS s" +
        " WHERE (s.name = ? AND s.email LIKE ? OR s.id IN ?)",
      Seq("Fabio", "epifab@%", List(1, 2, 6))
    )
  }

  it should "evaluate non trivial filters respecting precedence" in {
    import Filter._

    SelectQuery(students)

    val query =
      SelectQuery
        .from(students)
        .where(
          `student.name` === "Fabio"
            and
            (`student.email` like "epifab@%" or (`student.id` in List(1, 2, 6)))
        )

    select(query) shouldBe Query(
      "SELECT 1" +
        " FROM students AS s" +
        " WHERE s.name = ? AND (s.email LIKE ? OR s.id IN ?)",
      Seq("Fabio", "epifab@%", List(1, 2, 6))
    )
  }

  it should "evaluate left and inner joins" in {
    import Filter._

    val query =
      SelectQuery
        .from(students)
        .leftJoin(exams, `exams.student_id` === `student.id`)
        .innerJoin(course, `course.id` === `exams.course_id`)
        .take(`student.name`, `exams.rate`, `course_name`)

    select(query) shouldBe Query(
      "SELECT s.name AS s__name, e.rate AS e__rate, c.name AS c__name" +
        " FROM students AS s" +
        " LEFT JOIN exams AS e ON e.student_id = s.id" +
        " INNER JOIN courses AS c ON c.id = e.course_id" +
        " WHERE 1 = 1"
    )
  }
}

