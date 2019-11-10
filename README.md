# TYDAL

TYDAL (pronounced *tidal* - `/ˈtʌɪd(ə)l/` and formerly YADL)
is a type safe PostgreSQL DSL for Scala.


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

```scala
Select
  .from(Students as "s")
  .join($ => (Exams as "e").on(_ ("student_id") === $("s", "id")))
  .join($ => (Courses as "c").on(_ ("id") === $("e", "course_id")))
  .take($ => (
    $("s", "id") as "sid",
    $("s", "name") as "sname",
    $("e", "score") as "score",
    $("e", "exam_timestamp") as "etime",
    $("c", "name") as "cname"
  ))
  .where(_("s", "id") in List(1, 2, 3))
  .sortBy($ => (Ascending($("sid")), Descending($("score"))))
  .compile
  .mapTo[StudentExam]
  .as[Vector]
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
