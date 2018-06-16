package io.epifab.yadl.examples

import cats.Applicative
import io.epifab.yadl.implicits._
import io.epifab.yadl.domain.{DALError, Insert, QueryRunner}

import scala.language.higherKinds

class CourseRepo[F[_]](implicit queryRunner: QueryRunner[F], a: Applicative[F]) {
  private lazy val courses = new Schema.CoursesTable("c")

  def create(course: Course): F[Either[DALError, Int]] =
    Insert.into(courses)
      .set(
        courses.id -> course.id,
        courses.name -> course.name
      )
      .execute()
}
