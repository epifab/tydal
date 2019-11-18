# TYDAL

TYDAL (pronounced *tidal* `/ˈtʌɪd(ə)l/` and formerly YADL)
is a *type safe* PostgreSQL DSL for Scala.


## Getting started

### Installation

Step 1. Add the JitPack repository to your build file

```
resolvers += "jitpack" at "https://jitpack.io"
```

Step 2. Add the dependency

```
libraryDependencies += "com.github.epifab" % "tydal" % "1.x-SNAPSHOT"	
```

### A basic example

```scala
import java.time.LocalDate
import java.util.UUID

import io.epifab.tydal._
import io.epifab.tydal.fields.{FieldDecoder, FieldEncoder}

import java.time.LocalDate
import java.util.UUID

case class Address(postcode: String, line1: String, line2: Option[String])

case class Student(
  id: UUID,
  name: String,
  email: Option[String],
  date_of_birth: LocalDate,
  address: Option[Address]
)

object Students extends TableBuilder["students", Student]

object Programme extends App {
  import io.circe.generic.auto._
  implicit val addressEncoder: FieldEncoder[Address] = FieldEncoder.jsonEncoder[Address]
  implicit val addressDecoder: FieldDecoder[Address] = FieldDecoder.jsonDecoder[Address]

  val connection = PostgresConnection(PostgresConfig.fromEnv())

  val createStudent =
    Insert
      .into(Students)
      .compile
      .withValues(Student(
        UUID.randomUUID,
        "Jack",
        Some("jack@tydal.io"),
        LocalDate.of(1970, 1, 1),
        Some(Address("7590", "Tydalsvegen 125", Some("Tydal, Norway")))
      ))

  val findStudents = 
    Select
      .from(Students as "s")
      .take(_("s").*)
      .where(ctx => ctx("s", "email") like "email" and (ctx("date_of_birth") < "max_dob"))
      .compile
      .withValues((
        "email" ~~> "%@tydal.io",
        "max_dob" ~~> LocalDate.of(1986, 1, 1)
      ))
      .mapTo[Student]
      .as[Vector]

  val program = (for {
    _ <- createStudent
    students <- findStudents
  } yield students).toIO(connection)

  program.unsafeRunSync()
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
    Where e1.exam_timestamp > '2010-01-01T00:00:00Z'::timestamp
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
Where s.date_of_birth > '1994-01-01'::date AND s.date_of_birth < '1998-12-31'::date
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
      .take($ => (
        $("e1", "student_id") as "sid",
        Max($("e1", "score")) as "score"
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
      .take($ => (
        $("e2", "student_id") as "sid",
        $("e2", "score") as "score",
        Max($("e2", "exam_timestamp")) as "etime"
      ))
      .groupBy($ => ($("e2", "student_id"), $("e2", "score")))
      .as("me2")
  )
  .on((me2, ctx) => me2("sid") === ctx("me1", "sid") and (me2("score") === ctx("me1", "score")))
  .innerJoin(Exams as "e")
  .on((e, ctx) => e("exam_timestamp") === ctx("me2", "etime") and (e("student_id") === ctx("me2", "sid")))
  .innerJoin(Courses as "c")
  .on(_("id") === _("e", "course_id"))
  .take($ => (
    $("s", "id")             as "sid",
    $("s", "name")           as "sname",
    $("e", "score")          as "score",
    $("e", "exam_timestamp") as "etime",
    $("c", "name")           as "cname"
  ))
  .where(ctx => ctx("s", "date_of_birth") > "student_min_dob" and (ctx("s", "date_of_birth") < "student_max_dob"))
  .sortBy($ => Descending($("score")) -> Ascending($("sname")))
  .compile
  .withValues((
    "exam_min_date" ~~> Instant.parse("2010-01-01T00:00:00Z"),
    "student_min_dob" ~~> LocalDate.of(1994, 1, 1),
    "student_max_dob" ~~> LocalDate.of(1998, 12, 31)
  ))
```

Please find more examples [here](src/test/scala/io/epifab/tydal/examples).


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
