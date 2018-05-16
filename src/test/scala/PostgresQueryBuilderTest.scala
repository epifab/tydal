import domain._
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class PostgresQueryBuilderTest extends FlatSpec {
  import PostgresQueryBuilder._
  import FieldExtractor._

  val students: DataSource = Table("students", "s")
  val studentId: Field[Int] = Field("id", students)
  val studentName: Field[String] = Field("name", students)
  val studentEmail: Field[String] = Field("email", students)

  val exams: DataSource = Table("exams", "e")
  val examRate: Field[Int] = Field("rate", exams)
  val examStudentId: Field[Int] = Field("student_id", exams)

  "PostgresQuery" should "evaluate a the simplest query" in {
    val query = SelectQuery(students)
    queryEval(query) shouldBe Evaluated(
      "SELECT 1" +
        " FROM students AS s" +
        " WHERE 1 = 1")
  }

  it should "evaluate a query with 1 field" in {
    val query = SelectQuery(students, Seq(studentName))
    queryEval(query) shouldBe Evaluated(
      "SELECT s.name AS s__name" +
        " FROM students AS s" +
        " WHERE 1 = 1")
  }

  it should "evaluate a query with 2 fields" in {
    val query = SelectQuery(students, Seq(studentName, studentEmail), Seq.empty, Seq.empty)
    queryEval(query) shouldBe Evaluated(
      "SELECT s.name AS s__name, s.email AS s__email" +
        " FROM students AS s" +
        " WHERE 1 = 1")
  }

  it should "evaluate a query with a filter" in {
    val query = SelectQuery(students, Seq(studentName),
      filters = Seq(
        Filter.Expression(
          Filter.Expression.Clause.Field(studentName),
          Filter.Expression.Clause.Value("Fabio"),
          Filter.Expression.Op.Equal
        )
      )
    )
    queryEval(query) shouldBe Evaluated(
      "SELECT s.name AS s__name" +
        " FROM students AS s" +
        " WHERE s.name = ?", Seq("Fabio"))
  }

  it should "evaluate a query with a join" in {
    val query = SelectQuery(students, Seq(studentName, examRate),
      joins = Seq(
        Join(exams, LeftJoin, Seq(Filter.Expression(
          Filter.Expression.Clause.Field(examStudentId),
          Filter.Expression.Clause.Field(studentId),
          Filter.Expression.Op.Equal
        )))
      )
    )
    queryEval(query) shouldBe Evaluated(
      "SELECT s.name AS s__name, e.rate AS e__rate" +
        " FROM students AS s" +
        " LEFT JOIN exams AS e ON e.student_id = s.id" +
        " WHERE 1 = 1")
  }

  it should "evaluate a query with a join with fixed parameter" in {
    val query = SelectQuery(students, Seq(studentName, examRate),
      joins = Seq(
        Join(exams, LeftJoin, Seq(
          Filter.And(
            Filter.Expression(
              Filter.Expression.Clause.Field(examStudentId),
              Filter.Expression.Clause.Field(studentId),
              Filter.Expression.Op.Equal
            ),
            Filter.Expression(
              Filter.Expression.Clause.Field(examRate),
              Filter.Expression.Clause.Value(30),
              Filter.Expression.Op.Equal
            )
          )
        ))
      )
    )
    queryEval(query) shouldBe Evaluated(
      "SELECT s.name AS s__name, e.rate AS e__rate" +
        " FROM students AS s" +
        " LEFT JOIN exams AS e ON e.student_id = s.id AND e.rate = ?" +
        " WHERE 1 = 1",
      Seq(30)
    )
  }

  it should "evaluate more concise filters syntax" in {
    import Filter._

    val query = SelectQuery(students, Seq(studentName, examRate),
      joins = Seq(Join(exams, LeftJoin, Seq(examStudentId `==?` studentId))),
      filters = Seq((studentName `==?` "Fabio") or (studentEmail `like?` "epifab@%"))
    )

    queryEval(query) shouldBe Evaluated(
      "SELECT s.name AS s__name, e.rate AS e__rate" +
        " FROM students AS s" +
        " LEFT JOIN exams AS e ON e.student_id = s.id" +
        " WHERE s.name = ? OR s.email LIKE ?",
      Seq("Fabio", "epifab@%")
    )
  }
}

