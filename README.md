# YADL: Yet Another Data Library

Scala library designed for running strongly typed SQL-like queries.

Currently, it only supports Postgres by using an asynchronous driver.


### Installation (sbt)

Step 1. Add the JitPack repository to your build file

```
resolvers += "jitpack" at "https://jitpack.io"
```

Step 2. Add the dependency

```
libraryDependencies += "com.github.epifab" % "yadl" % "master-SNAPSHOT"	
```


### Examples

```scala
def findExamsByDate(date: LocalDate): F[Either[DALError, Seq[Exam :: Course ::HNil]]] =
  Select
    .from(Exams)
    .innerJoin(Exams.course)
    .take(Exams.* ++ Exams.course.*)
    .where(Exams.dateTime >= Value(date.atStartOfDay) and Exams.dateTime < Value(date.plusDays(1).atStartOfDay))
    .sortBy(Exams.studentId.asc)
    .fetchMany(examCourseExtractor)
```

More examples [here](https://github.com/epifab/yadl/tree/master/src/main/scala/io/epifab/yadl/examples).
