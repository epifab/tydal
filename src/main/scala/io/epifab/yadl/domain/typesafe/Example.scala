package io.epifab.yadl.domain.typesafe

import io.epifab.yadl.domain.typesafe.Implicits._
import shapeless.{::, HNil}

object Example {
  class Students extends Table[
    "students",
      (Term[Int] AS "id") ::
      (Term[String] AS "name") ::
      HNil
  ] {
    def id = field["id"].get
    def name = field["name"].get
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
    def studentId = field["student_id"].get
    def courseId = field["course_id"].get
    def score = field["score"].get
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
    def id = field["id"].get
    def name = field["name"].get
  }

  object Courses {
    def as[T]: Courses AS T = (new Courses).as[T]
  }

  type MaxScoreSubQuery[E, C] = Select[HNil, AS[Exams, E] :: Aggregation[Int, Option[Int]] with Alias["max_score"] :: Aggregation[Int, Option[Int]] with Alias["course_id"] :: HNil, Term[Int] with Alias["student_id"] :: HNil, AS[Exams, E] :: HNil]

  def maxScoreSubQuery[A, E, C]: MaxScoreSubQuery[E, C] with Alias[A] =
    Select
      .from(Exams.as[E])
      .groupBy(ctx => ctx.source[Exams AS E].field["student_id"].get :: HNil)
      .take(ctx => ctx.source[Exams AS E] ::
        Max(ctx.source[Exams AS E].field["score"].get).as["max_score"] ::
        Min(ctx.source[Exams AS E].field["course_id"].get).as["course_id"] ::
        HNil)
      .as[A]

  val studentsSelect =
    Select
      .from(Students.as["s"])
      .join(ctx => maxScoreSubQuery["maxScore", "ee", "cc"]
        .on(_.source[Exams AS "ee"].field["student_id"].get === ctx.source[Students AS "s"].id))
      .join(ctx => Courses.as["c"].on(_.id ===
        ctx.source[MaxScoreSubQuery["ee", "cc"] AS "maxScore"].field["course_id"].get))
      .take(ctx =>
        ctx.source[Students AS "s"].* ::
        ctx.source[MaxScoreSubQuery["ee", "cc"] AS "maxScore"].* ::
        ctx.source[Courses AS "c"].*
      )
      .withPlaceholder[Int, "studentId"]
      .where(ctx => ctx.source[Students AS "s"].id === ctx.placeholder[Placeholder[Int] AS "studentId"])
}
