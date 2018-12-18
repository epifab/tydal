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


### Examples

Define your data:

```scala
object Schema {
  class ExamsTable extends Table("exams") {
    lazy val studentId: TableColumn[Int] = column("student_id")
    lazy val courseId: TableColumn[Int] = column("course_id")
    lazy val score: TableColumn[Int] = column("score")
    lazy val dateTime: TableColumn[LocalDateTime] = column("exam_timestamp")
  
    lazy val `*`: Seq[Column[_]] = Seq(studentId, courseId, score, dateTime)
  
    lazy val course: Relation[CoursesTable] = (new CoursesTable).on(_.id === courseId)
  
    lazy val student: Relation[StudentsTable] = (new StudentsTable).on(_.id === studentId)
  }
}
```

Define your queries:

```scala
trait ExamsRepo[F[_]] extends Repo[F] {
  val exams = new Schema.ExamsTable

  def findExamsByDate(date: LocalDate): F[Either[DALError, Seq[Exam :: Course ::HNil]]] =
    Select
      .from(exams)
      .innerJoin(exams.course)
      .take(exams.* ++ exams.course.*)
      .where(exams.dateTime >= Value(date.atStartOfDay) and exams.dateTime < Value(date.plusDays(1).atStartOfDay))
      .sortBy(exams.studentId.asc)
      .fetchMany(examCourseExtractor)
}
```

More examples [here](https://github.com/epifab/yadl/tree/master/src/main/scala/io/epifab/yadl/examples).
