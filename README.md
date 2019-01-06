# YADL: Yet Another Data Library

Scala library designed for running strongly typed SQL-like queries.

Currently, it only supports Postgres via a classic JDBC driver.  
It's possible to plug in a different driver by defining a new instance of the `QueryRunner` type class
(a trivial Postgres asynchronous runner was added to the library as an example).  
The cost of switching between different drivers is minimal 
thanks to the use of higher kinded types and a finally tagless approach.


### Installation (sbt)

Step 1. Add the JitPack repository to your build file

```
resolvers += "jitpack" at "https://jitpack.io"
```

Step 2. Add the dependency

```
libraryDependencies += "com.github.epifab" % "yadl" % "master-SNAPSHOT"	
```



### Basic example

```scala
import io.epifab.yadl.domain._
import io.epifab.yadl.implicits._
import shapeless.{HNil, ::}

case class Student(
  id: Int,
  name: String,
  email: Option[String],
  dateOfBirth: LocalDate,
  address: Option[Address],
  interests: Seq[Interest]
)

class StudentsTable extends Table("students") {
  lazy val id: Column[Int] = column("id")
  lazy val name: Column[String] = column("name")
  lazy val email: Column[Option[String]] = column("email")
  lazy val dateOfBirth: Column[LocalDate] = column("date_of_birth")
  lazy val interests: Column[Seq[Interest]] = column("interests")
  lazy val address: Column[Option[Address]] = column("address")

  lazy val `*`: Reader[Student] = Reader(
    students.id ::
    students.name ::
    students.email ::
    students.dateOfBirth ::
    students.address ::
    students.interests ::
    HNil
  )
}

class StudentsRepo[F[_]](implicit queryRunner: QueryRunner[F]) {
  private val students = new Schema.StudentsTable
  
  def findStudentByName(name: String): F[Either[DALError, Option[Student]]] =
    Select
      .from(students)
      .take(students.*)
      .where(students.name === Value(name))
      .fetchOne
}
```
