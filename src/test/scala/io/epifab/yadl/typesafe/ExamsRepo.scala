package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.Schema.{Exam, Exams}
import Implicits._

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
}
