package io.epifab.tydal.examples

import java.sql.Connection
import java.time.{Instant, LocalDate}
import java.util.concurrent.atomic.AtomicInteger

import io.epifab.tydal.examples.Model._
import io.epifab.tydal.runner.{DataError, DriverError}
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
    Some("john@tydal.com"),
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
    StudentsRepo.findStudentExams(Seq(2, 1)).transact(connection).unsafeRunSync() shouldBe Right(Seq(
      StudentExam(1, "John Doe", 24, exam1.timestamp, "Math"),
      StudentExam(2, "Jane Doe", 30, exam3.timestamp, "Astronomy"),
      StudentExam(2, "Jane Doe", 29, exam2.timestamp, "Math")
    ))
  }

  it should "create, update and get a student" in {
    val jim = john.copy(name = "Jim", email = Some("jim@tydal.com"))

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

  it should "filter students by optional parameters" in {
    val atomicInteger = new AtomicInteger(100)
    val id = atomicInteger.get()
    def newId = atomicInteger.addAndGet(1)

    val a24yo = LocalDate.now.minusYears(24)
    val a23yo = LocalDate.now.minusYears(23)
    val a22yo = LocalDate.now.minusYears(22)
    val a21yo = LocalDate.now.minusYears(21)
    val allInterests = Seq(Interest.Art, Interest.History, Interest.Math, Interest.Music)

    (for {
      // matching email, all interests
      _ <- StudentsRepo.add(Student(newId, s"James $id Doe", Some(s"james$id@tydal.com"), a24yo, None, allInterests))
      _ <- StudentsRepo.add(Student(newId, s"Jack $id Doe", Some(s"jack$id@tydal.com"), a23yo, None, allInterests))
      _ <- StudentsRepo.add(Student(newId, s"John $id Doe", Some(s"john$id@tydal.com"), a22yo, None, allInterests))
      _ <- StudentsRepo.add(Student(newId, s"Jane $id Doe", Some(s"jane$id@tydal.com"), a21yo, None, allInterests))
      // non matching email
      _ <- StudentsRepo.add(Student(newId, s"James $id Doe", Some(s"james${id}@roar.com"), a24yo, None, allInterests))
      _ <- StudentsRepo.add(Student(newId, s"Jack $id Doe", Some(s"jack${id}@roar.com"), a23yo, None, allInterests))
      _ <- StudentsRepo.add(Student(newId, s"John $id Doe", Some(s"john${id}@roar.com"), a22yo, None, allInterests))
      _ <- StudentsRepo.add(Student(newId, s"Jane $id Doe", Some(s"jane${id}@roar.com"), a21yo, None, allInterests))
      // matching email, only math
      _ <- StudentsRepo.add(Student(newId, s"James $id Doe", Some(s"james$id@tydal.com"), a24yo, None, Seq(Interest.Math)))
      _ <- StudentsRepo.add(Student(newId, s"Jack $id Doe", Some(s"jack$id@tydal.com"), a23yo, None, Seq(Interest.Math)))
      _ <- StudentsRepo.add(Student(newId, s"John $id Doe", Some(s"john$id@tydal.com"), a22yo, None, Seq(Interest.Math)))
      _ <- StudentsRepo.add(Student(newId, s"Jane $id Doe", Some(s"jane$id@tydal.com"), a21yo, None, Seq(Interest.Math)))

      // different surname

      // matching email, all interests
      _ <- StudentsRepo.add(Student(newId, s"James $id Roe", Some(s"james$id@tydal.com"), a24yo, None, allInterests))
      _ <- StudentsRepo.add(Student(newId, s"Jack $id Roe", Some(s"jack$id@tydal.com"), a23yo, None, allInterests))
      _ <- StudentsRepo.add(Student(newId, s"John $id Roe", Some(s"john$id@tydal.com"), a22yo, None, allInterests))
      _ <- StudentsRepo.add(Student(newId, s"Jane $id Roe", Some(s"jane$id@tydal.com"), a21yo, None, allInterests))
      // non matching email
      _ <- StudentsRepo.add(Student(newId, s"James $id Roe", Some(s"james${id}@roar.com"), a24yo, None, allInterests))
      _ <- StudentsRepo.add(Student(newId, s"Jack $id Roe", Some(s"jack${id}@roar.com"), a23yo, None, allInterests))
      _ <- StudentsRepo.add(Student(newId, s"John $id Roe", Some(s"john${id}@roar.com"), a22yo, None, allInterests))
      _ <- StudentsRepo.add(Student(newId, s"Jane $id Roe", Some(s"jane${id}@roar.com"), a21yo, None, allInterests))
      // matching email, only math
      _ <- StudentsRepo.add(Student(newId, s"James $id Roe", Some(s"james$id@tydal.com"), a24yo, None, Seq(Interest.Math)))
      _ <- StudentsRepo.add(Student(newId, s"Jack $id Roe", Some(s"jack$id@tydal.com"), a23yo, None, Seq(Interest.Math)))
      _ <- StudentsRepo.add(Student(newId, s"John $id Roe", Some(s"john$id@tydal.com"), a22yo, None, Seq(Interest.Math)))
      _ <- StudentsRepo.add(Student(newId, s"Jane $id Roe", Some(s"jane$id@tydal.com"), a21yo, None, Seq(Interest.Math)))
    } yield ()).transact(connection).unsafeRunSync() shouldBe Symbol("Right")

    val results = for {
      i1 <- StudentsRepo.findAllBy(
        minAge = Some(22),
      ).map(_.length)

      i2 <- StudentsRepo.findAllBy(
        minAge = Some(22),
        maxAge = Some(23),
      ).map(_.length)

      i3 <- StudentsRepo.findAllBy(
        minAge = Some(22),
        maxAge = Some(23),
        name = Some("%Doe%"),
      ).map(_.length)

      i4 <- StudentsRepo.findAllBy(
        minAge = Some(22),
        maxAge = Some(23),
        name = Some("%Doe%"),
        email = Some(s"%@tydal.com")
      ).map(_.length)

      i5 <- StudentsRepo.findAllBy(
        minAge = Some(22),
        maxAge = Some(23),
        name = Some("%Doe%"),
        email = Some(s"%@tydal.com"),
        interests = Some(Seq(Interest.Music))
      ).map(_.length)
    } yield (i1, i2, i3, i4, i5)

    results.transact(connection).unsafeRunSync() shouldBe Right((21, 12, 6, 4, 2))
  }

  it should "filter students who have at least one good score" in {
    StudentsRepo
      .findStudentsWithAtLeast1ExamScore(28)
      .transact(connection)
      .unsafeRunSync() shouldBe Right(Set(student2))
  }

  //  it can "inject and extract all sort of fields" in {
//    getFields.transact(connection).unsafeRunSync() shouldBe Right(Seq(
//      (1, Seq(3.0, 9.99), Map("blue" -> "sky", "yellow" -> "banana"), LocalDate.of(1992, 2, 25), Instant.parse("1986-03-08T09:00:00z"))
//    ))
//  }
}
