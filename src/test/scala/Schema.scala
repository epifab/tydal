import io.epifab.dal.domain.{Filter, Relation, Table, TableField}

object Schema {
  import io.epifab.dal.Implicits._

  object students extends Table("hd_students", "s") {
    lazy val id: TableField[Int] = field("id")
    lazy val name: TableField[String] = field("name")
    lazy val email: TableField[Option[String]] = field("email")

    object exams extends Table("hd_exams", "e") with Relation {
      override val relationClause: Filter = studentId === students.id

      lazy val studentId: TableField[Int] = field("student_id")
      lazy val courseId: TableField[Int] = field("course_id")
      lazy val rate: TableField[Int] = field("rate")

      object course extends Table("hd_courses", "c") with Relation {
        override val relationClause: Filter = id === students.exams.courseId

        lazy val id: TableField[Int] = field("id")
        lazy val name: TableField[String] = field("name")
      }
    }
  }
}
