package io.epifab.yadl.examples

import io.epifab.yadl.examples.Model.Course
import io.epifab.yadl.examples.Schema.Courses
import io.epifab.yadl.runner.TransactionIO
import io.epifab.yadl.{Delete, Insert}
import io.epifab.yadl.Implicits._

object CoursesRepo {
  def add(course: Course): TransactionIO[Int] =
    Insert
      .into(Courses)
      .compile
      .withValues((
        "id" ~~> course.id,
        "name" ~~> course.name
      ))

  lazy val removeAll: TransactionIO[Int] =
    Delete
      .from(Courses)
      .compile
      .withValues(())
}
