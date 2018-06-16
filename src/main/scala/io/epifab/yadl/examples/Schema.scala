package io.epifab.yadl.examples

import io.epifab.yadl.domain._

object Schema {
  import io.epifab.yadl.implicits._

  class StudentsTable(val alias: String) extends Table { students =>
    override val src: String = "students"

    lazy val id: TableField[Int] = field("id")
    lazy val name: TableField[String] = field("name")
    lazy val email: TableField[Option[String]] = field("email")

    lazy val exams: ExamsTable with Relation = new ExamsTable(students.alias + "__exams") with Relation {
      override def relationClause: Filter = students.id === studentId
    }
  }

  class CoursesTable(val alias: String) extends Table {
    override def src: String = "courses"

    lazy val id: TableField[Int] = field("id")
    lazy val name: TableField[String] = field("name")
  }

  class ExamsTable(val alias: String) extends Table { exams =>
    override def src: String = "exams"

    lazy val studentId: TableField[Int] = field("student_id")
    lazy val courseId: TableField[Int] = field("course_id")
    lazy val rate: TableField[Int] = field("rate")

    lazy val course: CoursesTable with Relation = new CoursesTable(exams.alias + "__course") with Relation {
      override def relationClause: Filter = id === exams.courseId
    }

    lazy val student: StudentsTable with Relation = new StudentsTable(exams.alias + "__student") with Relation {
      override def relationClause: Filter = id === exams.studentId
    }
  }
}
