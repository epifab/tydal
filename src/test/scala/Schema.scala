import io.epifab.dal.domain.{Table, TableField}

object Schema {
  object students extends Table("hd_students", "s") {
    lazy val id: TableField[Int] = field("id")
    lazy val name: TableField[String] = field("name")
    lazy val email: TableField[String] = field("email")

    object exams extends Table("hd_exams", "e") {
      lazy val studentId: TableField[Int] = field("student_id")
      lazy val courseId: TableField[Int] = field("course_id")
      lazy val rate: TableField[Int] = field("rate")

      object course extends Table("hd_courses", "c") {
        lazy val id: TableField[Int] = field("id")
        lazy val name: TableField[String] = field("name")
      }
    }
  }
}
