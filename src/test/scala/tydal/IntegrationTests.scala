package tydal

import java.time.{Instant, LocalDate}
import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger

import tydal.queries.Select
import tydal.schema.{FieldDecoder, NamedPlaceholder}
import tydal.university.Model._
import tydal.university.{CoursesRepo, ExamsRepo, StudentsRepo}
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, FlatSpec}

class IntegrationTests extends FlatSpec with FunctionalTestBase with BeforeAndAfterAll {

  val student1 = Student(UUID.randomUUID(), "John Doe", Some("john@doe.com"), LocalDate.of(1974, 12, 14), Some(Address("N1001", "1 Fake St.", None)), Seq(Interest.Art, Interest.Math))
  val student2 = Student(UUID.randomUUID(), "Jane Doe", Some("jane@doe.com"), LocalDate.of(1986, 3, 8), Some(Address("N1002", "2 Fake St.", None)), Seq(Interest.Art, Interest.Music))
  val student3 = Student(UUID.randomUUID(), "Jack Roe", None, LocalDate.of(1992, 2, 25), Some(Address("N1003", "Fake St.", None)), Seq(Interest.Music))
  val course1 = Course(UUID.randomUUID(), "Math")
  val course2 = Course(UUID.randomUUID(), "Astronomy")
  val course3 = Course(UUID.randomUUID(), "Music")

  val john = Student(
    UUID.randomUUID(),
    "John",
    Some("john@tydal.com"),
    LocalDate.of(1986, 3, 8),
    Some(Address("N1 987", "32 Liverpool Road", Some("Hackney"))),
    Seq(Interest.Art)
  )

  private val marchThe8th: Instant = Instant.parse("2018-03-08T09:05:00z")
  private val novemberThe22nd3PM: Instant = Instant.parse("2018-11-22T15:30:20z")
  private val novemberThe22nd5PM: Instant = Instant.parse("2018-11-22T17:30:20z")

  val exam1 = Exam(student_id = student1.id, course_id = course1.id, 24, marchThe8th, Some(marchThe8th.plusNanos(123456000)))
  val exam2 = Exam(student_id = student2.id, course_id = course1.id, 29, novemberThe22nd3PM, Some(novemberThe22nd3PM.plusNanos(123456000)))
  val exam3 = Exam(student_id = student2.id, course_id = course2.id, 30, novemberThe22nd5PM, None)

  val student1Exams: Seq[(Exam, Course)] = Seq(exam1 -> course1)
  val student2Exams: Seq[(Exam, Course)] = Seq(exam2 -> course1, exam3 -> course2)
  val student3Exams: Seq[(Exam, Course)] = Seq.empty

  def tearDown(): Unit = (for {
    _ <- ExamsRepo.removeAll
    _ <- CoursesRepo.removeAll
    _ <- StudentsRepo.removeAll
  } yield ()).unsafeRun()

  def setUp(): Unit = (for {
    _ <- StudentsRepo.add(student1)
    _ <- StudentsRepo.add(student2)
    _ <- StudentsRepo.add(student3)
    _ <- CoursesRepo.add(course1)
    _ <- CoursesRepo.add(course2)
    _ <- CoursesRepo.add(course3)
    _ <- ExamsRepo.add(exam1)
    _ <- ExamsRepo.add(exam2)
    _ <- ExamsRepo.add(exam3)
  } yield ()).unsafeRun()

  override def beforeAll(): Unit = {
    tearDown()
    setUp()
  }

  override def afterAll(): Unit = {
    tearDown()
  }

  it should "run a query successfully" in {
    StudentsRepo.findStudentExams(Seq(student2.id, student1.id)).unsafeRun() shouldBe Seq(
      StudentExam(student2.id, "Jane Doe", 30, exam3.exam_timestamp, "Astronomy"),
      StudentExam(student2.id, "Jane Doe", 29, exam2.exam_timestamp, "Math"),
      StudentExam(student1.id, "John Doe", 24, exam1.exam_timestamp, "Math")
    )
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
    } yield (maybeJohn, maybeJim, maybeNobody)).unsafeRun()

    actualStudents shouldBe ((Some(john), Some(jim), None))
  }

  it should "rollback a transaction" in {
    val error = (for {
      _ <- StudentsRepo.add(john)
      _ <- StudentsRepo.add(john)
    } yield ()).run()

    error match {
      case Right(_) => fail("Duplicate key shouldn't be allowed")
      case Left(error) => error.getMessage should include("duplicate key value violates unique constraint")
    }

    StudentsRepo.findById(john.id).unsafeRun() shouldBe None
  }

//  it should "find a student by email" in {
//    StudentsRepo.findAllBy(_.email, student1.email)
//      .transact[IO](connection)
//      .unsafeRunSync() shouldBe Right(Seq(student1))
//  }

  it should "filter students by optional parameters" in {
    val atomicInteger = new AtomicInteger(100)
    val id = atomicInteger.get()
    def newId = {
      atomicInteger.incrementAndGet()
      UUID.randomUUID()
    }

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
    } yield ()).unsafeRun()

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

    results.unsafeRun() shouldBe (21, 12, 6, 4, 2)
  }

  it should "filter students who have at least one good score" in {
    StudentsRepo
      .findStudentsWithAtLeast1ExamScore(28)
      .unsafeRun() shouldBe Set(student2)
  }

  it should "filter students having less than 2 exams" in {
    StudentsRepo
      .findStudentsWithAtLeast2Exams
      .unsafeRun() shouldBe Set(student2.id -> Some(29.5))
  }

  it should "find users with their best exam" in {
    val jack = Student(UUID.randomUUID(), "Jack", None, LocalDate.of(1994, 2, 2), None, Seq.empty)
    val jackExam1 = Exam(jack.id, course1.id, 29, Instant.now.minusSeconds(172800), None)
    val jackExam2 = Exam(jack.id, course2.id, 29, Instant.now.minusSeconds(86400), None)
    val jackExam3 = Exam(jack.id, course3.id, 28, Instant.now, None)

    (for {
      _ <- StudentsRepo.add(jack)
      _ <- ExamsRepo.add(jackExam1)
      _ <- ExamsRepo.add(jackExam2)
      _ <- ExamsRepo.add(jackExam3)
    } yield ()).unsafeRun()

    val bestStudentExams = StudentsRepo
      .findStudentsWithBestExam
      .unsafeRun()
    bestStudentExams shouldBe Seq(StudentExam(jack.id, jack.name, 29, jackExam2.exam_timestamp, course2.name))
  }

  it should "encode and decode a bunch of fields" in {
    // todo: these two decoder should not be needed, but without them I get a divergent implicit expansion error
    implicit val optionalSequence: FieldDecoder[Option[Seq[Int]]] = FieldDecoder[Seq[Int]].toOption
    implicit def optionalOption[T](implicit b: FieldDecoder[T]): FieldDecoder[Option[Option[T]]] = b.toOption.toOption

    val foo = Select
      .take(_ => (
        NamedPlaceholder[Seq[Int], "list"],
        NamedPlaceholder[Option[Double], "double"],
        NamedPlaceholder[Option[Seq[Int]], "maybelist"],
        NamedPlaceholder[Option[Option[Seq[Int]]], "stupid"]
      ))
      .compile
      .toTuple
      .as[Vector]
      .run((
        "list" ~~> Seq(1, 2, 3),
        "double" ~~> Some(3.14),
        "maybelist" ~~> Some(Seq.empty[Int]),
        "stupid" ~~> Option[Option[Seq[Int]]](None)
      ))
      .unsafeRun()

    foo shouldBe Vector((
      Seq(1, 2, 3),
      Some(3.14),
      Some(Seq.empty),
      None
    ))
  }

  it should "encode and decode postgis fields" in {
    import tydal.schema.Postgis._

    val distances =
      Select
        .take1(_ =>
          Distance(
            MakePoint(
              NamedPlaceholder[Option[Double], "lat1"],
              NamedPlaceholder[Double, "lng1"]
            ).castTo[Geography],
            MakePoint(
              NamedPlaceholder[Double, "lat2"],
              NamedPlaceholder[Double, "lng2"]
            ).castTo[Geography]
          ) as "d"
        )
        .compile
        .toTuple
        .as[Vector]
        .run((
          "lat1" ~~> Some(51.5432), "lng1" ~~> 0.1519,
          "lat2" ~~> 51.4656, "lng2" ~~> 0.1150
        ))
        .unsafeRun()

    distances shouldBe Vector(Tuple1(Some(9553.50085968)))
  }
}
