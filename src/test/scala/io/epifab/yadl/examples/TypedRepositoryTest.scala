package io.epifab.yadl.examples

import java.time.{LocalDate, LocalDateTime}

import cats.Id
import org.scalatest.{BeforeAndAfterAll, FlatSpec}
import QueryRunnerFactories._
import io.epifab.yadl.domain.Delete
import org.scalatest.Matchers._
import shapeless.HNil


class TypedRepositoryTest extends FlatSpec with BeforeAndAfterAll {
  val repo = new TypedRepository[Id]

  val student = Student(1, "John Doe", Some("john@doe.com"), LocalDate.of(1974, 12, 14), Some(Address("N1001", "1 Fake St.", None)), Seq(Interest.Art, Interest.Math))
  val course1 = Course(1, "Math")
  val course2 = Course(2, "Astronomy")
  val exam1 = Exam(studentId = 1, courseId = 1, 24, LocalDateTime.of(2018, 3, 8, 9, 5, 6, 0))
  val exam2 = Exam(studentId = 1, courseId = 2, 29, LocalDateTime.of(2018, 11, 22, 15, 30, 20, 0))

  override def beforeAll(): Unit = {
    val results = for {
      _ <- syncQueryRunner.run(Delete(new Schema.ExamsTable))
      _ <- syncQueryRunner.run(Delete(new Schema.StudentsTable))
      _ <- syncQueryRunner.run(Delete(new Schema.CoursesTable))
      _ <- repo.createStudent(student)
      _ <- repo.createCourse(course1)
      _ <- repo.createCourse(course2)
      _ <- repo.createExam(exam1)
      _ <- repo.createExam(exam2)
    } yield Unit

    results shouldBe 'Right
  }

  override def afterAll(): Unit = {
    syncQueryRunner.run(Delete(new Schema.ExamsTable))
    syncQueryRunner.run(Delete(new Schema.StudentsTable))
    syncQueryRunner.run(Delete(new Schema.CoursesTable))
  }

  "The TypedSelect" should "fetch one student" in {
    repo.findStudent(student.id) shouldBe Right(Some(student))
  }

  it should "fetch exams and courses" in {
    repo.findExamsByStudentId(student.id) shouldBe Right(Seq(
      exam1 :: course1 :: HNil,
      exam2 :: course2 :: HNil
    ))
  }
}
