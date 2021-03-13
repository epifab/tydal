# TYDAL [![Build Status](https://travis-ci.com/epifab/tydal.svg?branch=1.x)](https://travis-ci.com/epifab/tydal)

TYDAL (pronounced *tidal* `/ˈtʌɪd(ə)l/` and formerly YADL)
is a *type-safe* SQL DSL and functional JDBC layer for Scala.


## Why Tydal

In a typical real-life application, the set of possible data access queries (e.g. SQL queries)
can be defined at edit-time. This means that their correctness can be -to certain extent-
verified before a program even runs (e.g. compile time).

Tydal offers a composable type-safe SQL-like DSL and a JDBC runtime environment
following canonical functional programming principles.

```scala
case class Book(isbn: String, name: String, authors: List[String])

object book extends TableSchema["book", (
  "isbn" :=: String,
  "name" :=: String,
  "authors" :=: List[String]
)]

val statement: ReadStatement[("isbn" ~~> String, "author" ~~> String), Book, Vector] = 
  Select
    .from(book as "b")
    .take(_("b").*)
    .where(_("b", "isbn") === "isbn")
    .orWhere(_("b", "authors") contains "author")
    .compile
    .to[Book]
    .as[Vector]

val books: IO[Vector[Book]] = statement
  .run((
    "isbn" ~~> "073521610X",
    "author" ~~> "Carlo"
  ))
  .transact(connectionPool)
```

A few nasty things tydal will help you to fight at a first glance:

- Typos, missing fields or relations:

```scala
Select.from(book as "b").take(_("b", "asd"))
// Field or relation "asd" could not be found
```

- Incompatible types comparison:

```scala
Select.from(book as "b").where(_("b", "isbn") === Literal(123))
// Column[String] and PlaceholderValue[Int] are not comparable

Select.from(book as "b").where(_("b", "isbn") in Literal(Seq(1999, 1998)))
// Column[String] cannot be included in PlaceholderValue[Seq[Int]]
```

- Incorrect query to model mapping:

```scala
Select.from(book as "b").take(_("b").*).mapTo[String]
// Could not find implicit for parameter generic ...
// ok, this last error message is not as specific as the ones above but it does the job
```


#### Supported DBMS

The only supported DBMS is *PostgreSQL*.


#### Supported SQL

Some -or even most- applications needs will be satisfied with the current DSL,
but SQL is huge, and Tydal only covers a handful of its features.  
You might (or might not) find this DSL limiting for your use-case.  
Some libraries (Slick or Doobie for example) allow plain text queries, but Tydal doesn't,
because that would make composability and some type constraints currently impossible to achieve
and ultimately defeat the purpose of this library.

Tydal is open source, feel free to contribute.


#### Supported types

Out of the box, the following field types are supported:

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


## Getting started

### Installation (sbt)

Step 1. Add the JitPack repository to your build file

```
resolvers += "jitpack" at "https://jitpack.io"
```

Step 2. Add the dependency

```
libraryDependencies += "com.github.epifab" % "tydal" % "1.3.7"	
```

### A basic app example

```scala
import java.time.LocalDate
import java.util.UUID

import cats.effect.{ExitCode, IO, IOApp}
import tydal._
import tydal.queries.{Insert, Select}
import tydal.runtime.{ConnectionPool, DataError, PostgresConfig}
import tydal.schema._

import scala.concurrent.ExecutionContext

case class Address(postcode: String, line1: String, line2: Option[String])

case class Student(
  id: UUID,
  name: String,
  email: Option[String],
  date_of_birth: LocalDate,
  address: Option[Address]
)

object Schema {
  import io.circe.generic.auto._
  implicit val addressEncoder: FieldEncoder.Aux[Address, String] = FieldEncoder.jsonEncoder[Address]
  implicit val addressDecoder: FieldDecoder.Aux[Address, String] = FieldDecoder.jsonDecoder[Address]

  object students extends TableBuilder["students", (
    "id" :=: UUID,
    "name" :=: String,
    "email" :=: Option[String],
    "date_of_birth" :=: LocalDate,
    "address" :=: Option[Address]
  )]
}

object Program extends IOApp {
  import Schema._

  val createStudent =
    Insert
      .into(students)
      .compile
      .run((
        "id" ~~> UUID.randomUUID,
        "name" ~~> "Jack",
        "email" ~~> Some("jack@tydal.io"),
        "date_of_birth" ~~> LocalDate.of(1970, 1, 1),
        "address" ~~> Some(Address("7590", "Tydalsvegen 125", Some("Tydal, Norway"))),
      ))

  val findStudents =
    Select
      .from(students as "s")
      .take(_("s").*)
      .focus("s").where(s => (s("email") like "email") and (s("date_of_birth") < "max_dob"))
      .compile
      .to[Student]
      .as[Vector]
      .run((
        "email" ~~> "%@tydal.io",
        "max_dob" ~~> LocalDate.of(1986, 1, 1)
      ))


  val connectionPool = ConnectionPool.resource[IO](
    PostgresConfig.fromEnv(),
    PoolConfig(maxPoolSize = Some(2))
  )

  override def run(args: List[String]): IO[ExitCode] = {
    connectionPool.use { pool =>
      val result = (for {
        _ <- createStudent
        students <- findStudents
      } yield students).transact(pool).attempt

      result.map(_.fold(_ => ExitCode.Error, _ => ExitCode.Success))
    }
  }
}
```

### A not-so-basic example

**In English**

```
Find all students born between 1994 and 1998 who have taken at least one exam since 2010.
Also, find their best and most recent exam
```

**In SQL**

```sql
SELECT
    s.id             AS sid,
    s.name           AS sname,
    e.score          AS escore,
    e.exam_timestamp AS etime,
    c.name           AS cname
FROM students AS s
INNER JOIN (
    SELECT 
        e1.student_id AS sid,
        MAX(s1.score) AS score
    FROM exams AS e1
    WHERE e1.exam_timestamp > '2010-01-01T00:00:00Z'::timestamp
    GROUP BY e1.student_id
) AS se1
ON se1.student_id = s.id
INNER JOIN (
    SELECT
        e2.student_id          AS sid,
        e2.score               AS score
        MAX(e2.exam_timestamp) AS etime
    FROM exams AS e2
    GROUP BY e2.student_id, e2.score
) AS se2
ON se2.student_id = se1.student_id AND se2.score = se1.score
INNER JOIN exams AS e
ON e.student_id = se2.student_id AND e.exam_timestamp = se2.etime
INNER JOIN courses AS c
ON c.id = e.course_id
WHERE s.date_of_birth > '1994-01-01'::date AND s.date_of_birth < '1998-12-31'::date
ORDER BY escore DESC, sname ASC
```

**In Scala**

```scala
Select
  .from(Students as "s")
  .innerJoin(
    // max score per student
    Select
      .from(Exams as "e1")
      .take(ctx => (
        ctx("e1", "student_id") as "sid",
        Max(ctx("e1", "score")) as "score"
      ))
      .where(_("e1", "exam_timestamp") > "exam_min_date")
      .groupBy1(_("e1", "student_id"))
      .as("me1")
  )
  .on(_("sid") === _("s", "id"))
  .innerJoin(
    // select only the latest exam
    Select
      .from(Exams as "e2")
      .take(ctx => (
        ctx("e2", "student_id")          as "sid",
        ctx("e2", "score")               as "score",
        Max(ctx("e2", "exam_timestamp")) as "etime"
      ))
      .groupBy(ctx => (ctx("e2", "student_id"), ctx("e2", "score")))
      .as("me2")
  )
  .on((me2, ctx) => me2("sid") === ctx("me1", "sid") and (me2("score") === ctx("me1", "score")))
  .innerJoin(Exams as "e")
  .on((e, ctx) => e("exam_timestamp") === ctx("me2", "etime") and (e("student_id") === ctx("me2", "sid")))
  .innerJoin(Courses as "c")
  .on(_("id") === _("e", "course_id"))
  .take(ctx => (
    ctx("s", "id")             as "sid",
    ctx("s", "name")           as "sname",
    ctx("e", "score")          as "score",
    ctx("e", "exam_timestamp") as "etime",
    ctx("c", "name")           as "cname"
  ))
  .where(ctx => ctx("s", "date_of_birth") > "student_min_dob" and (ctx("s", "date_of_birth") < "student_max_dob"))
  .sortBy(ctx => Descending(ctx("score")) -> Ascending(ctx("sname")))
```

Please find more examples [here](src/test/scala/tydal/university).
