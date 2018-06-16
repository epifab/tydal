package io.epifab.yadl.examples

import cats.Applicative
import io.epifab.yadl.domain.{DALError, Insert, QueryRunner}

import scala.language.higherKinds

class CourseRepo[F[_]](queryRunner: QueryRunner[F])(a: Applicative[F]) {
  private lazy val courses = new Schema.CoursesTable("c")

  def create(course: Course): F[Either[DALError, Int]] = {
    val query = Insert.into(courses)
      .set(
        courses.id -> course.id,
        courses.name -> course.name
      )

    queryRunner.run(query)
  }
}
