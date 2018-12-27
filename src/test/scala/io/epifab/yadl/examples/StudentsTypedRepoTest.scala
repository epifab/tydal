package io.epifab.yadl.examples

import java.time.LocalDate

import cats.Id
import org.scalatest.{BeforeAndAfterAll, FlatSpec}
import QueryRunnerFactories._
import io.epifab.yadl.domain.Delete
import org.scalatest.Matchers._


class StudentsTypedRepoTest extends FlatSpec with BeforeAndAfterAll {
  val studentsRepo = new TypedStudentsRepo[Id]

  val student = Student(1, "John Doe", Some("john@doe.com"), LocalDate.of(1974, 12, 14), Some(Address("N1001", "1 Fake St.", None)), Seq(Interest.Art, Interest.Math))

  override def beforeAll(): Unit = {
    syncQueryRunner.run(Delete(new Schema.StudentsTable)) shouldBe 'Right
    studentsRepo.createStudent(student) shouldBe 'Right
  }

  override def afterAll(): Unit = {
    syncQueryRunner.run(Delete(new Schema.StudentsTable))
  }

  "The TypedSelect" should "work perfectly" in {
    studentsRepo.findStudent(student.id) shouldBe Right(Some(student))
  }
}
