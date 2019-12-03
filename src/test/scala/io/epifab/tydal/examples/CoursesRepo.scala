package io.epifab.tydal.examples

import io.epifab.tydal._
import io.epifab.tydal.queries._
import io.epifab.tydal.examples.Model.Course
import io.epifab.tydal.examples.Schema.Courses
import io.epifab.tydal.runtime.Transaction

object CoursesRepo {
  def add(course: Course): Transaction[Int] =
    Insert
      .into(Courses)
      .compile
      .run((
        "id" ~~> course.id,
        "name" ~~> course.name
      ))

  lazy val removeAll: Transaction[Int] =
    Delete
      .from(Courses)
      .compile
      .run(())
}
