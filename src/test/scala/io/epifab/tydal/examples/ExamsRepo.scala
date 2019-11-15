package io.epifab.tydal.examples

import io.epifab.tydal.examples.Model.Exam
import io.epifab.tydal.examples.Schema.Exams
import io.epifab.tydal.runner.Transaction
import io.epifab.tydal.{Delete, Insert}
import io.epifab.tydal.Implicits._

object ExamsRepo {
  def add(exam: Exam): Transaction[Int] =
    Insert
      .into(Exams)
      .compile
      .withValues((
        "student_id" ~~> exam.studentId,
        "course_id" ~~> exam.courseId,
        "score" ~~> exam.score,
        "exam_timestamp" ~~> exam.timestamp,
        "registration_timestamp" ~~> exam.registration
      ))

  lazy val removeAll: Transaction[Int] =
    Delete
      .from(Exams)
      .compile
      .withValues(())
}
