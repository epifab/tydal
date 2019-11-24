package io.epifab.tydal.examples

import io.epifab.tydal._
import io.epifab.tydal.queries._
import io.epifab.tydal.examples.Model.Exam
import io.epifab.tydal.examples.Schema.Exams
import io.epifab.tydal.runtime.Transaction

object ExamsRepo {
  def add(exam: Exam): Transaction[Int] =
    Insert
      .into(Exams)
      .compile
      .withValues((
        "student_id" ~~> exam.student_id,
        "course_id" ~~> exam.course_id,
        "score" ~~> exam.score,
        "exam_timestamp" ~~> exam.exam_timestamp,
        "registration_timestamp" ~~> exam.registration_timestamp
      ))

  lazy val removeAll: Transaction[Int] =
    Delete
      .from(Exams)
      .compile
      .withValues(())
}
