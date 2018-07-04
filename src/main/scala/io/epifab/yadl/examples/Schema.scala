package io.epifab.yadl.examples

import io.epifab.yadl.domain._

object Schema {
  import io.circe.generic.auto._
  import io.epifab.yadl.implicits._

  case class Address(postcode: String, line1: String, line2: Option[String])

  class StudentsTable(val alias: String) extends Table { self =>
    override val src: String = "students"

    lazy val id: TableField[Int] = field("id")
    lazy val name: TableField[String] = field("name")
    lazy val email: TableField[Option[String]] = field("email")
    lazy val interests: TableField[Seq[String]] = field("interests")
    lazy val address: TableField[Option[Json[Address]]] = field("address")

    lazy val `*`: Seq[TableField[_]] = Seq(id, name, email)

    lazy val exams: ExamsTable with Relation = new ExamsTable(self.alias + "__exams") with Relation {
      override def relationClause: Filter = self.id === studentId
    }
  }

  class CoursesTable(val alias: String) extends Table { self =>
    override def src: String = "courses"

    lazy val id: TableField[Int] = field("id")
    lazy val name: TableField[String] = field("name")

    lazy val `*`: Seq[TableField[_]] = Seq(id, name)

    lazy val exams: ExamsTable with Relation = new ExamsTable(self.alias + "__exams") with Relation {
      override def relationClause: Filter = self.id === courseId
    }
  }

  class ExamsTable(val alias: String) extends Table { self =>
    override def src: String = "exams"

    lazy val studentId: TableField[Int] = field("student_id")
    lazy val courseId: TableField[Int] = field("course_id")
    lazy val rate: TableField[Int] = field("rate")

    lazy val `*`: Seq[TableField[_]] = Seq(studentId, courseId, rate)

    lazy val course: CoursesTable with Relation = new CoursesTable(self.alias + "__course") with Relation {
      override def relationClause: Filter = id === self.courseId
    }

    lazy val student: StudentsTable with Relation = new StudentsTable(self.alias + "__student") with Relation {
      override def relationClause: Filter = id === self.studentId
    }
  }
}
