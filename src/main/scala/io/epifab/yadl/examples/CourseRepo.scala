package io.epifab.yadl.examples

import io.epifab.yadl.domain.{DALError, Insert}
import io.epifab.yadl.implicits._

import scala.language.higherKinds

trait CourseRepo[F[_]] extends Repo[F] {
  object Courses extends Schema.CoursesTable("c")

  def createCourse(course: Course): F[Either[DALError, Int]] =
    Insert.into(Courses)
      .set(
        Courses.id -> course.id,
        Courses.name -> course.name
      )
      .execute()
}
