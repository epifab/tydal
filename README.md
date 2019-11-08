# TYDAL

TYDAL pronounced *tidal* - `/ˈtʌɪd(ə)l/` and formerly YADL)
is a compile time PostgreSQL interpreter* for Scala. 


## Getting started

### Installation

Step 1. Add the JitPack repository to your build file

```
resolvers += "jitpack" at "https://jitpack.io"
```

Step 2. Add the dependency

```
libraryDependencies += "com.github.epifab" % "tydl" % "1.x-SNAPSHOT"	
```


### What does it look like?

- The Model:

```scala
object Model {
  abstract sealed class Interest(val value: String)

  object Interest {
    def apply(value: String): Either[String, Interest] = value match {
      case Music.value => Right(Music)
      case Art.value => Right(Art)
      case History.value => Right(History)
      case Math.value => Right(Math)
      case _ => Left("Unknown interest")
    }

    case object Music extends Interest("music")
    case object Art extends Interest("art")
    case object History extends Interest("history")
    case object Math extends Interest("math")
  }

  case class Address(postcode: String, line1: String, line2: Option[String])

  case class Student(
    id: Int,
    name: String,
    email: Option[String],
    dateOfBirth: LocalDate,
    address: Option[Address],
    interests: Seq[Interest]
  )
}
```

- The Schema:

```scala
import io.epifab.tydal._

object Schema {
  implicit val interestDecoder: FieldDecoder.Aux[Interest, String] =
    FieldDecoder.enumDecoder("interest", Interest.apply)

  implicit val interestEncoder: FieldEncoder.Aux[Interest, String] =
    FieldEncoder.enumEncoder("interest", _.value)

  implicit val addressDecoder: FieldDecoder.Aux[Address, String] =
    FieldDecoder.jsonDecoder[Address]

  implicit val addressEncoder: FieldEncoder.Aux[Address, String] =
    FieldEncoder.jsonEncoder[Address]

  case class StudentsSchema(
    id: Column[Int] AS "id",
    name: Column[String] AS "name",
    email: Column[Option[String]] AS "email",
    dateOfBirth: Column[LocalDate] AS "date_of_birth",
    address: Column[Option[Address]] AS "address",
    interests: Column[Seq[Interest]] AS "interests"
  )

  object Students extends TableBuilder["students", StudentsSchema]
}
```

- The Repository:

```scala
object StudentsRepo {
  def findById(id: Int): TransactionIO[Option[Student]] =
    Select
      .from(Students as "s")
      .take(_("s").*)
      .where(_("s", "id") === "student_id")
      .compile
      .withValues(Tuple1("student_id" ~~> id))
      .mapTo[Student]
      .option

  def updateNameAndEmail(id: Int, name: String, email: Option[String]): TransactionIO[Int] =
    Update(Students)
      .fields(s => (s.name, s.email))
      .where(_.id === "id")
      .compile
      .withValues {
        (
          "name" ~~> name,
          "email" ~~> email,
          "id" ~~> id
        )
      }

  def add(student: Student): TransactionIO[Int] = {
    Insert
      .into(Students)
      .compile
      .withValues {
        (
          "id" ~~> student.id,
          "name" ~~> student.name,
          "email" ~~> student.email,
          "date_of_birth" ~~> student.dateOfBirth,
          "address" ~~> student.address,
          "interests" ~~> student.interests
        )
      }
  }
}
```

- The Program:
```scala
object UpdateStudentProgram extends App {
  private val connection: java.sql.Connection = ???

  private val studentProgram: TransactionIO[Int] = StudentsRepo.findById(123) flatMap {
    case Some(student) =>
      StudentsRepo.updateNameAndEmail(student.id, "James Doe", Some("james.doe@tydl.com"))
    case None =>
      StudentsRepo.add(Student(123, "John Doe", Some("john.doe@tydl.org")))
  }
  
  studentProgram.transact(connection).unsafeRunSync()
}
```

Find more examples [here](src/main/scala/io/epifab/tydal/examples).


## More in-depth


### Query DSL

The idea behind this library is to provide a DSL as close as possible to the SQL language.

Different features are supported although the library does not cover the entire SQL universe nor has the ambition to do so.
You can explore some functionalities in the [examples package](src/main/scala/io/epifab/tydal/examples).


### Supported DBMS and data types

Currently, the only supported DBMS is **PostgreSQL** with the following data types:

Database type               | Scala type
---                         | ---
`char`, `varchar`, `text`   | `String`
`int`                       | `Int`
`float`                     | `Double`
`date`, `timestamp`         | `LocalDate`, `Instant` (java.time)
`enum`                      | Any type `T`
`arrays`                    | `Seq[T]` where `T` is any of the above
`json`                      | Any type `T`

In addition, every data type can be made optional and encoded as `Option`.

The mapping between a SQL data type and its Scala representation is defined via two *type classes* named `FieldEncoder` and `FieldDecoder`.
You can, of course, define new encoders/decoders in order to support custom types.
