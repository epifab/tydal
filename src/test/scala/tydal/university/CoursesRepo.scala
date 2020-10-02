package tydal.university

import tydal._
import Model.Course
import Schema.Courses
import tydal.queries._
import tydal.runtime.Transaction

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
