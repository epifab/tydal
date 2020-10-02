package tydal.university

import Model.Exam
import Schema._
import tydal.queries._
import tydal.runtime.Transaction

object ExamsRepo {
  def add(exam: Exam): Transaction[Int] =
    Insert
      .into(Exams)
      .compile
      .runP(exam)

  lazy val removeAll: Transaction[Int] =
    Delete
      .from(Exams)
      .compile
      .run(())
}
