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

Define your schema:

```scala
object Schema {
  class ExamsTable extends Table("exams") {
    lazy val studentId: Column[Int] = column("student_id")
    lazy val courseId: Column[Int] = column("course_id")
    lazy val score: Column[Int] = column("score")
    lazy val dateTime: Column[LocalDateTime] = column("exam_timestamp")
  
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
      .fetchMany
}
```

More examples [here](https://github.com/epifab/yadl/tree/master/src/main/scala/io/epifab/yadl/examples).


### Typed DSL

*Experimental feature*

Manually declaring extractors can be tedious and error-prone.
I'm currently working on a typed DSL which will solve this and other issues.

Here's a (working!) example:

The model:

```scala
case class Student(
  id: Int,
  name: String,
  email: Option[String],
  dateOfBirth: LocalDate,
  address: Option[Address],
  interests: Seq[Interest]
)
```

The schema:

```scala
class StudentsTable extends Table("students") {
  lazy val id: Column[Int] = column("id")
  lazy val name: Column[String] = column("name")
  lazy val email: Column[Option[String]] = column("email")
  lazy val dateOfBirth: Column[LocalDate] = column("date_of_birth")
  lazy val interests: Column[Seq[Interest]] = column("interests")
  lazy val address: Column[Option[Address]] = column("address")
}
```

Link the two things:

```scala
val studentsRepr(students: StudentsTable) =
  (students.id +:
    students.name +:
    students.email +:
    students.dateOfBirth +:
    students.address +:
    students.interests +:
    SNil).as[Student]
```

Try it out:

```scala
val students = new Schema.StudentsTable

def findAllStudents: F[Either[DALError, Seq[Student]]] =
  TypedSelect
    .from(students)
    .take(studentsRepr(students))
    .fetchMany
```
