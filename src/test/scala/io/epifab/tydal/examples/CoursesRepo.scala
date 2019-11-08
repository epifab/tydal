package io.epifab.tydal.examples

import io.epifab.tydal.examples.Model.Course
import io.epifab.tydal.examples.Schema.Courses
import io.epifab.tydal.runner.TransactionIO
import io.epifab.tydal.{Delete, Insert}
import io.epifab.tydal.Implicits._

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
