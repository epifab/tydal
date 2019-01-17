# YADL: Yet Another Data Library

Scala library designed for running strongly typed SQL queries.


## Getting started

### Installation

Step 1. Add the JitPack repository to your build file

```
resolvers += "jitpack" at "https://jitpack.io"
```

Step 2. Add the dependency

```
libraryDependencies += "com.github.epifab" % "yadl" % "master-SNAPSHOT"	
```

Please note, the following dependencies will be included:

- Cats (for applicatives, monads and more)
- Shapeless (for hlists and automatic type class derivation)
- Circe (for JSON support)


### What does it look like?

```scala
import io.epifab.yadl.domain._
import io.epifab.yadl.implicits._
import io.epifab.yadl.postgres._
import shapeless.{::, HNil}

case class Student(id: Int, name: String, email: Option[String])

class StudentsTable extends Table[Student]("students") {
  val id: Column[Int] = column("id")
  val name: Column[String] = column("name")
  val email: Column[Option[String]] = column("email")

  lazy val `*`: Columns[Student] = Columns(id :: name :: email :: HNil)
}

class StudentsRepo[F[_]: cats.Applicative](implicit queryRunner: QueryRunner[F]) {
  private val students = new StudentsTable

  def findById(id: Int): F[Either[DALError, Option[Student]]] =
    Select
      .from(students)
      .take(students.*)
      .where(students.id === Value(id))
      .fetchOne

  def update(student: Student): F[Either[DALError, Int]] =
    Update(students)
      .set(student)
      .where(students.id === Value(student.id))
      .execute()

  def insert(student: Student): F[Either[DALError, Int]] =
    Insert
      .into(students)
      .set(student)
      .execute()
}

object UpdateStudentProgram extends App {
  implicit val connection: java.sql.Connection = ???

  val repository = new StudentsRepo[cats.Id]

  repository.findById(123) flatMap {
    case Some(student) =>
      repository.update(student.copy(name = "John Doe"))
    case None =>
      repository.insert(Student(123, "John Doe", Some("john.doe@yadl.org")))
  }
}
```

Find more examples [here](src/main/scala/io/epifab/yadl/examples).


## More in-depth


### Query DSL

The idea behind this library was to provide a DSL as close as possible to the SQL language:

```scala
Select(exams.*, exams.course.*, exams.course.professor.*)
  .from(exams)
  .innerJoin(exams.course)
  .innerJoin(exams.course.professor)
  .where(exams.studentId === Value(123) and (exams.dateTime <= Value(now.atStartOfDay) and exams.dateTime > Value(now.minusDays(1).asStartOfDay)))
  .sortBy(exams.studentId.asc)
  .inRange(0, 100)
```

Different features are supported although the library does not cover the entire SQL universe nor has the ambition to do so.
You can explore some functionalities in the [examples package](src/main/scala/io/epifab/yadl/examples).


### Supported DBMS and data types

Currently, the only supported DBMS is **PostgreSQL** with the following data types:

Database type               | Scala type
---                         | ---
`char`, `varchar`, `text`   | `String`
`int`                       | `Int`
`float`                     | `Double`
`date`, `timestamp`         | `LocalDate`, `LocalDateTime` (java.time)
`enum`                      | Any type `T`
`arrays`                    | `Seq[T]` where `T` is any of the above
`json`                      | Any type `T`

In addition, every data type can be made optional and encoded as `Option`.

The mapping between a SQL data type and its Scala representation is defined via a *type class* named `FieldAdapter`.
As you would expect, you can create new `FieldAdapter` instances in order to support custom types.

Here's a couple of examples:

```scala
implicit def booleanFieldAdapter(implicit intFieldAdapter: FieldAdapter[Int]): FieldAdapter[Boolean] =
  intFieldAdapter.bimap(
    (b: Boolean) => if (b) 1 else 0, 
    (i: Int) => i != 0
  )
```

```scala
import io.circe.generic.auto._

case class Address(postcode: String, line1: String, line2: Option[String])

implicit def addressFieldAdapter: FieldAdapter[Address] = new JsonFieldAdapter
```

### Synchronous vs. asynchronous drivers

You probably have noticed by now the extensive use of *higher kinded types*:

```scala
class StudentsRepo[F[_]: cats.Applicative](implicit queryRunner: QueryRunner[F])
```

This design has the important consequence of making the library driver-agnostic.
In fact, it is rather painless to switch between a blocking and a non-blocking implementation 
or even to something more sophisticated such as an IO monad.

Different solutions can be implemented by defining new instances of the [`QueryRunner`](src/main/scala/io/epifab/yadl/domain/QueryRunner.scala) type class.
