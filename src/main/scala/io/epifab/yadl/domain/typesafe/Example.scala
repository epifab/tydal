package io.epifab.yadl.domain.typesafe

import Implicits._
import shapeless.{::, HNil}

object Example {
  class Students extends Table[
    "students",
      (Term[Int] AS "id") ::
      (Term[String] AS "name") ::
      HNil
  ] {
    def id: Term[Int] AS "id" = *.select[Term[Int] AS "id"]
    def name: Term[String] AS "name" = *.select[Term[String] AS "name"]
  }

  object Students {
    def as[T]: Students AS T = (new Students).as[T]
  }

  class Exams extends Table[
    "exams",
      (Term[Int] AS "student_id") ::
      (Term[Int] AS "course_id") ::
      (Term[Int] AS "score") ::
      HNil
  ] {
    val studentId: Term[Int] AS "student_id" = *.select[Term[Int] AS "student_id"]
    val courseId: Term[Int] AS "course_id" = *.select[Term[Int] AS "course_id"]
    val score: Term[Int] AS "score" = *.select[Term[Int] AS "score"]
  }

  object Exams {
    def as[T]: Exams AS T = (new Exams).as[T]
  }

  class Courses extends Table[
    "courses",
      (Term[Int] AS "id") ::
      (Term[String] AS "name") ::
      HNil
  ] {
    def id: Term[Int] AS "id" = *.select[Term[Int] AS "id"]
    def name: Term[String] AS "name" = *.select[Term[String] AS "name"]
  }

  object Courses {
    def as[T]: Courses AS T = (new Courses).as[T]
  }

  type MaxScoreSubQuery[E, C] = Select[HNil, AS[Exams, E] :: Aggregation[Int, Option[Int]] with Alias["max_score"] :: Aggregation[Int, Option[Int]] with Alias["course_id"] :: HNil, Term[Int] with Alias["student_id"] :: HNil, AS[Exams, E] :: HNil]

  def maxScoreSubQuery[A, E, C]: MaxScoreSubQuery[E, C] with Alias[A] =
    Select
      .from(Exams.as[E])
      .groupBy(ctx => ctx.source[Exams AS E].term[Term[Int] AS "student_id"] :: HNil)
      .take(ctx => ctx.source[Exams AS E] ::
        Max(ctx.source[Exams AS E].term[Term[Int] AS "score"]).as["max_score"] ::
        Min(ctx.source[Exams AS E].term[Term[Int] AS "course_id"]).as["course_id"] ::
        HNil)
      .as[A]

  val studentsSelect =
    Select
      .from(Students.as["s"])
      .join(ctx => maxScoreSubQuery["maxScore", "ee", "cc"]
        .on(_.source[Exams AS "ee"].studentId === ctx.source[Students AS "s"].id))
      .join(ctx => Courses.as["c"].on(_.id ===
        ctx.source[MaxScoreSubQuery["ee", "cc"] AS "maxScore"].term[Aggregation[Int, Option[Int]] AS "course_id"]))
      .take(ctx =>
        ctx.source[Students AS "s"].* ::
        ctx.source[MaxScoreSubQuery["ee", "cc"] AS "maxScore"].* ::
        ctx.source[Courses AS "c"].*
      )
      .withPlaceholder[Int, "studentId"]
      .where(ctx => ctx.source[Students AS "s"].id === ctx.placeholder[Placeholder[Int] AS "studentId"])
}
