package io.epifab.yadl.examples

import java.time.{LocalDate, LocalDateTime}

import io.epifab.yadl.domain._

object Schema {
  import io.circe.generic.auto._
  import io.epifab.yadl.implicits._

  class StudentsTable(override val tableAlias: String) extends Table("students") { self =>
    lazy val id: TableColumn[Int] = column("id")
    lazy val name: TableColumn[String] = column("name")
    lazy val email: TableColumn[Option[String]] = column("email")
    lazy val dateOfBirth: TableColumn[LocalDate] = column("date_of_birth")
    lazy val interests: TableColumn[Seq[String]] = column("interests")
    lazy val address: TableColumn[Option[Json[Address]]] = column("address")

    lazy val `*`: Seq[TableColumn[_]] = Seq(id, name, email, dateOfBirth, interests, address)

    lazy val exams: ExamsTable with Relation = new ExamsTable(self.tableAlias + "__exams") with Relation {
      override def relationClause: Filter = self.id === studentId
    }
  }

  class CoursesTable(override val tableAlias: String) extends Table("courses") { self =>

    lazy val id: TableColumn[Int] = column("id")
    lazy val name: TableColumn[String] = column("name")

    lazy val `*`: Seq[TableColumn[_]] = Seq(id, name)

    lazy val exams: ExamsTable with Relation = new ExamsTable(self.tableAlias + "__exams") with Relation {
      override def relationClause: Filter = self.id === courseId
    }
  }

  class ExamsTable(override val tableAlias: String) extends Table("exams") { self =>

    lazy val studentId: TableColumn[Int] = column("student_id")
    lazy val courseId: TableColumn[Int] = column("course_id")
    lazy val score: TableColumn[Int] = column("score")
    lazy val dateTime: TableColumn[LocalDateTime] = column("exam_timestamp")

    lazy val `*`: Seq[TableColumn[_]] = Seq(studentId, courseId, score, dateTime)

    lazy val course: CoursesTable with Relation = new CoursesTable(self.tableAlias + "__course") with Relation {
      override def relationClause: Filter = id === self.courseId
    }

    lazy val student: StudentsTable with Relation = new StudentsTable(self.tableAlias + "__student") with Relation {
      override def relationClause: Filter = id === self.studentId
    }
  }
}
