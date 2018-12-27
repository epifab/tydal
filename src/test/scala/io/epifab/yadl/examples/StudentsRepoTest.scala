package io.epifab.yadl.examples

import java.time.{LocalDate, LocalDateTime}

import cats.Applicative
import cats.data.EitherT
import io.epifab.yadl.domain.{DALError, Delete, QueryRunner}
import io.epifab.yadl.utils.EitherSupport._
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, FlatSpec}
import shapeless._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class StudentsRepoTest extends FlatSpec with BeforeAndAfterAll {
  import cats.implicits._

  import scala.concurrent.ExecutionContext.Implicits.global

  implicit class ExtendedFuture[T](f: Future[T]) {
    def eventually: T = Await.result[T](f, 5.seconds)
  }

  val student1 = Student(1, "John Doe", Some("john@doe.com"), LocalDate.of(1974, 12, 14), Some(Address("N1001", "1 Fake St.", None)), Seq(Interest.Art, Interest.Math))
  val student2 = Student(2, "Jane Doe", Some("jane@doe.com"), LocalDate.of(1986, 3, 8), Some(Address("N1002", "2 Fake St.", None)), Seq(Interest.Art, Interest.Music))
  val student3 = Student(3, "Jack Roe", None, LocalDate.of(1992, 2, 25), Some(Address("N1003", "Fake St.", None)), Seq(Interest.Music))
  val course1 = Course(1, "Math")
  val course2 = Course(2, "Astronomy")
  val exam1 = Exam(studentId = 1, courseId = 1, 24, LocalDateTime.of(2018, 3, 8, 9, 5, 6, 0))
  val exam2 = Exam(studentId = 2, courseId = 1, 29, LocalDateTime.of(2018, 11, 22, 15, 30, 20, 0))
  val exam3 = Exam(studentId = 2, courseId = 2, 30, LocalDateTime.of(2018, 11, 22, 17, 30, 20, 0))

  val student1Exams: Seq[Exam :: Course :: HNil] = Seq(exam1 :: course1 :: HNil)
  val student2Exams: Seq[Exam :: Course :: HNil] = Seq(exam2 :: course1 :: HNil, exam3 :: course2 :: HNil)
  val student3Exams: Seq[Exam :: Course :: HNil] = Seq.empty

  object repos extends StudentsRepo[Future] with ExamsRepo[Future] with CoursesRepo[Future] {
    override implicit val queryRunner: QueryRunner[Future] = QueryRunnerFactories.asyncQueryRunner
    override implicit val A: Applicative[Future] = implicitly
  }

  def tearDown(): Future[Either[DALError, Int]] = {
    repos.queryRunner.run(Delete(new Schema.ExamsTable))
      .flatMap(_ => repos.queryRunner.run(Delete(new Schema.CoursesTable)))
      .flatMap(_ => repos.queryRunner.run(Delete(new Schema.StudentsTable)))
  }

  def setUp(): Future[Either[DALError, Seq[Int]]] = {
    Future.sequence(Seq(
      repos.createStudent(student1),
      repos.createStudent(student2),
      repos.createStudent(student3),
      repos.createCourse(course1),
      repos.createCourse(course2),
      repos.createExam(exam1),
      repos.createExam(exam2),
      repos.createExam(exam3)
    )).map(firstLeftOrRights)
  }

  override def beforeAll(): Unit = {
    tearDown().eventually shouldBe 'Right
    setUp().eventually shouldBe 'Right
  }

  override def afterAll(): Unit = {
    tearDown().eventually shouldBe 'Right
  }

  "The query runner" should "retrieve a student by ID" in {
    repos.findStudent(2).eventually shouldBe Right(Some(student2))
  }

  it should "retrieve a list of students by name" in {
    repos.findStudentByName("%Doe").eventually shouldBe Right(Seq(student1, student2))
  }

  it should "retrieve a list of students by ids" in {
    repos.findStudents(2, 3, 4).eventually shouldBe Right(Seq(student2, student3))
  }

  it should "retrieve a list of students by email" in {
    repos.findStudentByEmail("%@doe.com").eventually shouldBe Right(Seq(student1, student2))
  }

  it should "retrieve students with missing email" in {
    repos.findStudentsWithoutEmail().eventually shouldBe Right(Seq(student3))
  }

  it should "find students by list of interests (contains)" in {
    repos.findStudentsByInterests(Seq.empty).eventually shouldBe Right(Seq(student1, student2, student3))
    repos.findStudentsByInterests(Seq(Interest.Music)).eventually shouldBe Right(Seq(student2, student3))
    repos.findStudentsByInterests(Seq(Interest.Music, Interest.Art)).eventually shouldBe Right(Seq(student2))
    repos.findStudentsByInterests(Seq(Interest.Music, Interest.Art, Interest.History)).eventually shouldBe Right(Seq.empty)
  }

  it should "find students by list of interests (intercepts)" in {
    repos.findStudentsByAnyInterest(Seq.empty).eventually shouldBe Right(Seq.empty)
    repos.findStudentsByAnyInterest(Seq(Interest.Music)).eventually shouldBe Right(Seq(student2, student3))
    repos.findStudentsByAnyInterest(Seq(Interest.Music, Interest.Art)).eventually shouldBe Right(Seq(student1, student2, student3))
    repos.findStudentsByAnyInterest(Seq(Interest.Music, Interest.Art, Interest.History)).eventually shouldBe Right(Seq(student1, student2, student3))
  }

  it should "find exams by date" in {
    repos.findExamsByDate(LocalDate.of(2018, 11, 22)).eventually shouldBe Right(Seq(
      exam2 :: course1 :: HNil,
      exam3 :: course2 :: HNil
    ))
  }

  it should "find student exams" in {
    repos.findStudentsExams(student1, student2, student3).eventually shouldBe Right(Seq(
      student1 :: student1Exams :: HNil,
      student2 :: student2Exams :: HNil,
      student3 :: student3Exams :: HNil
    ))
  }

  it should "find student exam stats" in {
    repos.findStudentExamStats(2).eventually shouldBe Right(
      Some(
        StudentExams(
          count=Some(2),
          avgScore=Some(29.5),
          minScore=Some(29),
          maxScore=Some(30)
        )
      )
    )
  }

  it should "find student exams stats (using a sub query)" in {
    repos.findStudentExamStats2(2).eventually shouldBe Right(Some(2, Some(29.5)))
  }

  it should "not find exams for unexisting student" in {
    repos.findStudentExamStats(123).eventually shouldBe Right(None)
  }

  it should "update a student" in {
    val edited: EitherT[Future, DALError, Option[Student]] = for {
      _ <- EitherT(repos.updateStudent(student3.copy(name = "Edited")))
      edited <- EitherT(repos.findStudent(3))
    } yield edited

    edited.value.eventually.map(_.map(_.name)) shouldBe Right(Some("Edited"))
  }

  it should "find students by date of birth (testing date sequences)" in {
    val results = repos.findStudentsByDateOfBirth(student1.dateOfBirth, student2.dateOfBirth)
    results.eventually.map(_.toSet) shouldBe Right(Set(student1, student2))
  }

  it should "find exams by date time (testing date time sequences)" in {
    val results = repos.findExamsByDateTime(exam1.dateTime, exam3.dateTime)
    results.eventually.map(_.toSet) shouldBe Right(Set(exam1, exam3))
  }
}

