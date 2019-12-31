package io.epifab.tydal.university

import Model.Exam
import Schema._
import io.epifab.tydal.queries._
import io.epifab.tydal.runtime.Transaction

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
