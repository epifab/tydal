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
  .innerJoin(
    // max score per student
    Select
      .from(Exams as "e1")
      .take(ctx => (
        ctx("e1").studentId  as "sid1",
        Max(ctx("e1").score) as "score1"
      ))
      .groupBy1(_("e1").studentId)
      .as("me1")
  )
  .on(_("sid1") === _("s").id)
  .innerJoin(
    // select only the latest exam
    Select
      .from(Exams as "e2")
      .take(ctx => (
        ctx("e2").studentId          as "sid2",
        ctx("e2").score              as "score2",
        Max(ctx("e2").examTimestamp) as "etime"
      ))
      .groupBy(ctx => (ctx("e2").studentId, ctx("e2").score))
      .as("me2")
  )
  .on((me2, ctx) =>
    me2("sid2") === ctx("me1", "sid1") and 
      (me2("score2") === ctx("me1", "score1")))
  .innerJoin(Exams as "e")
  .on((e, ctx) =>
    e.examTimestamp === ctx("me2", "etime") and 
      (e.studentId === ctx("me2", "sid2")))
  .innerJoin(Courses as "c")
  .on(_.id === _("e").courseId)
  .take(ctx => (
    ctx("s").id            as "sid",
    ctx("s").name          as "sname",
    ctx("e").score         as "score",
    ctx("e").examTimestamp as "etime",
    ctx("c").name          as "cname"
  ))
  .sortBy(ctx => Descending(ctx("score")) -> Ascending(ctx("sname")))
  .compile
  .withValues(())
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
