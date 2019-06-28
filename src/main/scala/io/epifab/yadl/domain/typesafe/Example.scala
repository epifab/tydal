package io.epifab.yadl.domain.typesafe

import Implicits._
import shapeless.{HNil, ::}

object Example {
  class Students extends Table[Term[Int] :: Term[String] :: HNil]("students") {
    val id: Term[Int] = term("id")
    val name: Term[String] = term("name")

    val `*`: Term[Int] :: Term[String] :: HNil = id :: name :: HNil
  }

  object Students {
    def as[T]: Students AS T = (new Students).as[T]
  }

  class Exams extends Table[Term[Int] :: Term[Int] :: Term[Int] :: HNil]("exams") {
    val studentId: Term[Int] = term("student_id")
    val courseId: Term[Int] = term("course_id")
    val score: Term[Int] = term("score")

    val `*`: Term[Int] :: Term[Int] :: Term[Int] :: HNil =
      studentId :: courseId :: score :: HNil
  }

  object Exams {
    def as[T]: Exams AS T = (new Exams).as[T]
  }

  class Courses extends Table[Term[Int] :: Term[String] :: HNil]("courses") {
    val id: Term[Int] = term("id")
    val name: Term[String] = term("name")

    val `*`: Term[Int] :: Term[String] :: HNil =
      id :: name :: HNil
  }

  object Courses {
    def as[T]: Courses AS T = (new Courses).as[T]
  }

  val examsSelect: Select[HNil, Term[Int] :: Aggregation[Int, Option[Int]] :: HNil, AS[Exams, "e"] :: HNil] =
    Select
      .from(Exams.as["e"])
      .take(ctx => ctx.dataSource[Exams AS "e"].studentId :: Max(ctx.dataSource[Exams AS "e"].score) :: HNil)

  val studentsSelect: Select[Placeholder[Int] with Alias["studentId"] :: HNil, Term[Int] :: Term[String] :: Term[Int] :: Term[String] :: HNil, Students with Alias["s"] :: Join[Exams with Alias["e"]] :: Join[Courses with Alias["c"]] :: HNil] =
    Select
      .from(Students.as["s"])
      .withPlaceholder[Int, "studentId"]
      .join(ctx => Exams.as["e"].on(_.studentId === ctx.dataSource[Students AS "s"].id))
      .join(ctx => Courses.as["c"].on(_.id === ctx.dataSource[Exams AS "e"].courseId))
      .take(ctx =>
        ctx.dataSource[Students AS "s"].* ++
        ctx.dataSource[Courses AS "c"].*
      )
      .where(ctx => ctx.dataSource[Students AS "s"].id === ctx.placeholder[Int, "studentId"])

  Select.from(Students.as["s"])
    .join(ctx => examsSelect.as["xx"].on(_.dataSource[Exams AS "e"].studentId === ctx.dataSource[Students AS "s"].id))
}
