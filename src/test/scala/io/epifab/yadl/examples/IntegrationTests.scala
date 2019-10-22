package io.epifab.yadl.examples

import java.time.{Instant, LocalDate, LocalDateTime, ZoneOffset}

import cats.Applicative
import io.epifab.yadl.{PostgresConfig, PostgresConnection}
import io.epifab.yadl.domain.{DALError, Delete, QueryRunner}
import io.epifab.yadl.examples.SelectsQueries.studentExams
import io.epifab.yadl.typesafe.DataError
import io.epifab.yadl.typesafe.fields.Value
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, FlatSpec}
import shapeless._

class Repos(override val queryRunner: QueryRunner[Id])(override implicit val A: Applicative[Id])
  extends StudentsRepo[Id]
    with ExamsRepo[Id]
    with CoursesRepo[Id]

class IntegrationTests extends FlatSpec with BeforeAndAfterAll {
  val student1 = Student(1, "John Doe", Some("john@doe.com"), LocalDate.of(1974, 12, 14), Some(Address("N1001", "1 Fake St.", None)), Seq(Interest.Art, Interest.Math))
  val student2 = Student(2, "Jane Doe", Some("jane@doe.com"), LocalDate.of(1986, 3, 8), Some(Address("N1002", "2 Fake St.", None)), Seq(Interest.Art, Interest.Music))
  val student3 = Student(3, "Jack Roe", None, LocalDate.of(1992, 2, 25), Some(Address("N1003", "Fake St.", None)), Seq(Interest.Music))
  val course1 = Course(1, "Math")
  val course2 = Course(2, "Astronomy")

  private val marchThe8th: LocalDateTime = LocalDateTime.of(2018, 3, 8, 9, 5, 6, 0)
  private val novemberThe22nd3PM: LocalDateTime = LocalDateTime.of(2018, 11, 22, 15, 30, 20, 0)
  private val novemberThe22nd5PM: LocalDateTime = LocalDateTime.of(2018, 11, 22, 17, 30, 20, 0)

  val exam1 = Exam(studentId = 1, courseId = 1, 24, marchThe8th, Some(marchThe8th.withNano(123456000).toInstant(ZoneOffset.UTC)))
  val exam2 = Exam(studentId = 2, courseId = 1, 29, novemberThe22nd3PM, Some(novemberThe22nd3PM.withNano(123456000).toInstant(ZoneOffset.UTC)))
  val exam3 = Exam(studentId = 2, courseId = 2, 30, novemberThe22nd5PM, None)

  val student1Exams: Seq[(Exam, Course)] = Seq(exam1 -> course1)
  val student2Exams: Seq[(Exam, Course)] = Seq(exam2 -> course1, exam3 -> course2)
  val student3Exams: Seq[(Exam, Course)] = Seq.empty

  val repo = new Repos(QueryRunnerFactories.syncQueryRunner)

  def tearDown(): Either[DALError, Unit] = for {
    _ <- repo.queryRunner.run(Delete(new Schema.ExamsTable))
    _ <- repo.queryRunner.run(Delete(new Schema.CoursesTable))
    _ <- repo.queryRunner.run(Delete(new Schema.StudentsTable))
  } yield ()

  def setUp(): Either[DALError, Unit] = for {
    _ <- repo.createStudent(student1)
    _ <- repo.createStudent(student2)
    _ <- repo.createStudent(student3)
    _ <- repo.createCourse(course1)
    _ <- repo.createCourse(course2)
    _ <- repo.createExam(exam1)
    _ <- repo.createExam(exam2)
    _ <- repo.createExam(exam3)
  } yield ()

  override def beforeAll(): Unit = {
    tearDown() shouldBe Symbol("Right")
    setUp() shouldBe Symbol("Right")
  }

  override def afterAll(): Unit = {
    tearDown() shouldBe Symbol("Right")
  }

  "The query runner" should "retrieve a student by ID" in {
    repo.findStudent(2) shouldBe Right(Some(student2))
  }

  it should "retrieve a list of students by name" in {
    repo.findStudentByName("%Doe") shouldBe Right(Seq(student1, student2))
  }

  it should "retrieve a list of students by ids" in {
    repo.findStudents(2, 3, 4) shouldBe Right(Seq(student2, student3))
  }

  it should "retrieve a list of students by email" in {
    repo.findStudentByEmail("%@doe.com") shouldBe Right(Seq(student1, student2))
  }

  it should "retrieve students with missing email" in {
    repo.findStudentsWithoutEmail() shouldBe Right(Seq(student3))
  }

  it should "find students by list of interests (contains)" in {
    repo.findStudentsByInterests(Seq.empty) shouldBe Right(Seq(student1, student2, student3))
    repo.findStudentsByInterests(Seq(Interest.Music)) shouldBe Right(Seq(student2, student3))
    repo.findStudentsByInterests(Seq(Interest.Music, Interest.Art)) shouldBe Right(Seq(student2))
    repo.findStudentsByInterests(Seq(Interest.Music, Interest.Art, Interest.History)) shouldBe Right(Seq.empty)
  }

  it should "find students by list of interests (intercepts)" in {
    repo.findStudentsByAnyInterest(Seq.empty) shouldBe Right(Seq.empty)
    repo.findStudentsByAnyInterest(Seq(Interest.Music)) shouldBe Right(Seq(student2, student3))
    repo.findStudentsByAnyInterest(Seq(Interest.Music, Interest.Art)) shouldBe Right(Seq(student1, student2, student3))
    repo.findStudentsByAnyInterest(Seq(Interest.Music, Interest.Art, Interest.History)) shouldBe Right(Seq(student1, student2, student3))
  }

  it should "find exams by date" in {
    repo.findExamsByDate(LocalDate.of(2018, 11, 22)) shouldBe Right(Seq(
      exam2 -> course1,
      exam3 -> course2
    ))
  }

  it should "find student exams" in {
    repo.findStudentsExams(student1, student2, student3) shouldBe Right(Seq(
      student1 -> student1Exams,
      student2 -> student2Exams,
      student3 -> student3Exams
    ))
  }

  it should "find student exam stats" in {
    repo.findStudentExamStats(2) shouldBe Right(
      Some(
        StudentExams(
          studentId = 2,
          examsCount = 2,
          avgScore = Some(29.5),
          minScore = Some(29),
          maxScore = Some(30)
        )
      )
    )
  }

  it should "not find exams for unexisting student" in {
    repo.findStudentExamStats(123) shouldBe Right(None)
  }

  it should "update a student" in {
    val edited: Either[DALError, Option[Student]] = for {
      _ <- repo.updateStudent(student3.copy(name = "Edited"))
      edited <- repo.findStudent(3)
    } yield edited

    edited.map(_.map(_.name)) shouldBe Right(Some("Edited"))
  }

  it should "find students by date of birth (testing date sequences)" in {
    val results = repo.findStudentsByDateOfBirth(student1.dateOfBirth, student2.dateOfBirth)
    results.map(_.toSet) shouldBe Right(Set(student1, student2))
  }

  it should "find exams by date time (testing date time sequences)" in {
    val results = repo.findExamsByDateTime(exam1.dateTime, exam3.dateTime)
    results.map(_.toSet) shouldBe Right(Set(exam1, exam3))
  }

  it should "find distinct courses" in {
    val results = repo.findCourseIdsByStudentExams(student1, student2, student3)
    results.map(_.map(_.id)) shouldBe Right(Seq(1, 2))
  }

  it should "run a query successfully" in {
    case class StudentExam(id: Int, name: String, score: Int, time: Instant, course: String)

    val students: Either[DataError, Seq[StudentExam]] =
      studentExams
        .withValues(Value("sid", 2) :: HNil)
        .runSync[StudentExam](PostgresConnection(PostgresConfig.fromEnv()))

    students.map(_.toSet) shouldBe Right(Set(
      StudentExam(2, "Jane Doe", 29, exam2.dateTime.toInstant(ZoneOffset.UTC), "Math"),
      StudentExam(2, "Jane Doe", 30, exam3.dateTime.toInstant(ZoneOffset.UTC), "Astronomy")
    ))
  }
}

