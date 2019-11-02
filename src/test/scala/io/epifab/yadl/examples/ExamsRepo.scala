package io.epifab.yadl.examples

import io.epifab.yadl.examples.Model.Exam
import io.epifab.yadl.examples.Schema.Exams
import io.epifab.yadl.runner.TransactionIO
import io.epifab.yadl.{Delete, Insert}
import io.epifab.yadl.Implicits._

object ExamsRepo {
  def add(exam: Exam): TransactionIO[Int] =
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

  lazy val removeAll: TransactionIO[Int] =
    Delete
      .from(Exams)
      .compile
      .withValues(())
}
