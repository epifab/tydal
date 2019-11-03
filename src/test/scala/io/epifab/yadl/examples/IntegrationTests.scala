package io.epifab.yadl.examples

import java.sql.Connection
import java.time.{Instant, LocalDate}

import io.epifab.yadl.examples.Model._
import io.epifab.yadl.runner.{DataError, DriverError}
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, FlatSpec}

class IntegrationTests extends FlatSpec with BeforeAndAfterAll {
  val student1 = Student(1, "John Doe", Some("john@doe.com"), LocalDate.of(1974, 12, 14), Some(Address("N1001", "1 Fake St.", None)), Seq(Interest.Art, Interest.Math))
  val student2 = Student(2, "Jane Doe", Some("jane@doe.com"), LocalDate.of(1986, 3, 8), Some(Address("N1002", "2 Fake St.", None)), Seq(Interest.Art, Interest.Music))
  val student3 = Student(3, "Jack Roe", None, LocalDate.of(1992, 2, 25), Some(Address("N1003", "Fake St.", None)), Seq(Interest.Music))
  val course1 = Course(1, "Math")
  val course2 = Course(2, "Astronomy")

  val john = Student(
    199,
    "John",
    Some("john@yadl.com"),
    LocalDate.of(1986, 3, 8),
    Some(Address("N1 987", "32 Liverpool Road", Some("Hackney"))),
    Seq(Interest.Art)
  )

  private val marchThe8th: Instant = Instant.parse("2018-03-08T09:05:00z")
  private val novemberThe22nd3PM: Instant = Instant.parse("2018-11-22T15:30:20z")
  private val novemberThe22nd5PM: Instant = Instant.parse("2018-11-22T17:30:20z")

  val exam1 = Exam(studentId = 1, courseId = 1, 24, marchThe8th, Some(marchThe8th.plusNanos(123456000)))
  val exam2 = Exam(studentId = 2, courseId = 1, 29, novemberThe22nd3PM, Some(novemberThe22nd3PM.plusNanos(123456000)))
  val exam3 = Exam(studentId = 2, courseId = 2, 30, novemberThe22nd5PM, None)

  val student1Exams: Seq[(Exam, Course)] = Seq(exam1 -> course1)
  val student2Exams: Seq[(Exam, Course)] = Seq(exam2 -> course1, exam3 -> course2)
  val student3Exams: Seq[(Exam, Course)] = Seq.empty

  def tearDown(): Either[DataError, Unit] = (for {
    _ <- ExamsRepo.removeAll
    _ <- CoursesRepo.removeAll
    _ <- StudentsRepo.removeAll
  } yield ()).transact(connection).unsafeRunSync()

  def setUp(): Either[DataError, Unit] = (for {
    _ <- StudentsRepo.add(student1)
    _ <- StudentsRepo.add(student2)
    _ <- StudentsRepo.add(student3)
    _ <- CoursesRepo.add(course1)
    _ <- CoursesRepo.add(course2)
    _ <- ExamsRepo.add(exam1)
    _ <- ExamsRepo.add(exam2)
    _ <- ExamsRepo.add(exam3)
  } yield ()).transact(connection).unsafeRunSync()

  override def beforeAll(): Unit = {
    tearDown() shouldBe Symbol("Right")
    setUp() shouldBe Symbol("Right")
  }

  override def afterAll(): Unit = {
    tearDown() shouldBe Symbol("Right")
  }

  private val connection: Connection = QueryRunnerFactories.connection

  it should "run a query successfully" in {
    StudentsRepo.findStudentExams(2).transact(connection).unsafeRunSync().map(_.toSet) shouldBe Right(Set(
      StudentExam(2, "Jane Doe", 29, exam2.timestamp, "Math"),
      StudentExam(2, "Jane Doe", 30, exam3.timestamp, "Astronomy")
    ))
  }

  it should "create, update and get a student" in {
    val jim = john.copy(name = "Jim", email = Some("jim@yadl.com"))

    val findStudent = StudentsRepo.findById(john.id)

    val actualStudents = (for {
      _ <- StudentsRepo.add(john)
      maybeJohn <- findStudent
      _ <- StudentsRepo.updateNameAndEmail(john.id, jim.name, jim.email)
      maybeJim <- findStudent
      _ <- StudentsRepo.remove(john.id)
      maybeNobody <- findStudent
    } yield (maybeJohn, maybeJim, maybeNobody)).transact(connection).unsafeRunSync()

    actualStudents shouldBe Right((Some(john), Some(jim), None))
  }

  it should "rollback a transaction" in {
    (for {
      _ <- StudentsRepo.add(john)
      _ <- StudentsRepo.add(john)
    } yield ()).transact(connection).unsafeRunSync() shouldBe Left(DriverError("ERROR: duplicate key value violates unique constraint \"students_pkey\"\n  Detail: Key (id)=(199) already exists."))

    StudentsRepo.findById(john.id).transact(connection).unsafeRunSync() shouldBe Right(None)
  }

  it should "find a student by email" in {
    StudentsRepo.findAllBy(_.email, student1.email)
      .transact(connection)
      .unsafeRunSync() shouldBe Right(Seq(student1))
  }

  //  it can "inject and extract all sort of fields" in {
//    getFields.transact(connection).unsafeRunSync() shouldBe Right(Seq(
//      (1, Seq(3.0, 9.99), Map("blue" -> "sky", "yellow" -> "banana"), LocalDate.of(1992, 2, 25), Instant.parse("1986-03-08T09:00:00z"))
//    ))
//  }
}
