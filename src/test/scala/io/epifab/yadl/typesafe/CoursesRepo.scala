package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.Schema.{Course, Courses}
import Implicits._

object CoursesRepo {
  def add(course: Course): TransactionIO[Int] =
    Insert
      .into(Courses)
      .compile
      .withValues((
        "id" ~~> course.id,
        "name" ~~> course.name
      ))
}
