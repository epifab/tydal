package io.epifab.yadl.examples

import io.epifab.yadl.examples.TypesafeSchema.{Courses, Exams, Students, studentsSelect}
import io.epifab.yadl.typesafe.Implicits.ExtendedField
import io.epifab.yadl.typesafe._
import io.epifab.yadl.typesafe.fields.{AlwaysTrue, Column, Field}
import org.scalatest.{FlatSpec, Matchers}
import Implicits._
import shapeless.{::, HNil}

object SelectsQueries {
  val examsWithCourse =
    Select
      .from(Exams.as["e"])
      .join($ => Courses.as["c"].on(_["id"].get === $["e", "course_id"].get))
      .take($ =>
        $["c", "name"].get.as["cname"] ::
        $["e", "score"].get.as["score"] ::
        HNil)
}

class TypesafeTest extends FlatSpec with Matchers {
  "The TagMap typeclass" should "bind fields to their alias" in {
    val fields: Map[String, Field[Any]] =
      TagMap(studentsSelect.fields)

    fields.keys.toSet shouldBe Set("sid", "sname", "score", "cname")
  }

  "The source finder" should "get any source" in {
    val select: SelectContext[HNil, Table["students", _] with Tag["s"] :: Join[Table["exams", _] with Tag["e"]] :: Join[Table["courses", _] with Tag["c"]] :: HNil] = new SelectContext[HNil, Table["students", _] with Tag["s"] :: Join[Table["exams", _] with Tag["e"]] :: Join[Table["courses", _] with Tag["c"]] :: HNil] {
      override val fields: HNil = HNil
      override val sources: Table["students", _] with Tag["s"] :: Join[Table["exams", _] with Tag["e"]] :: Join[Table["courses", _] with Tag["c"]] :: HNil =
        Students.as["s"] ::
          new Join(Exams.as["e"], AlwaysTrue) ::
          new Join(Courses.as["c"], AlwaysTrue) ::
          HNil
    }

    select["s"].get shouldBe a[Table["students", _] with Tag["s"]]
    select["e"].get shouldBe a[Table["exams", _] with Tag["e"]]
    select["c"].get shouldBe a[Table["courses", _] with Tag["c"]]
  }

  "The QueryBuilder" should "build a simple query" in {
    QueryBuilder(studentsSelect["ms"].get.select) shouldBe
      Some("SELECT" +
        " e.student_id AS student_id," +
        " max(e.score) AS max_score," +
        " min(e.course_id) AS course_id" +
        " FROM exams AS e" +
        " GROUP BY e.student_id")
  }

  it should "build a query with join" in {
    QueryBuilder(SelectsQueries.examsWithCourse) shouldBe Some(
      "SELECT" +
        " c.name AS cname," +
        " e.score AS score" +
        " FROM exams AS e" +
        " INNER JOIN courses AS c ON c.id = e.course_id"
    )
  }

  it should "build a query with subqueries" in {
    val subQuery =
      "SELECT e.student_id AS student_id," +
        " max(e.score) AS max_score," +
        " min(e.course_id) AS course_id" +
        " FROM exams AS e" +
        " GROUP BY e.student_id"

    QueryBuilder(studentsSelect) shouldBe Some(
      "SELECT" +
        " s.id AS sid," +
        " s.name AS sname," +
        " ms.max_score AS score," +
        " cc.name AS cname" +
        " FROM students AS s" +
        " INNER JOIN (" + subQuery + ") AS ms ON ms.student_id = s.id" +
        " INNER JOIN courses AS cc ON cc.id = ms.course_id" +
        " WHERE s.id = :student_id"
    )
  }
}
