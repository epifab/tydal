package io.epifab.yadl.examples

import io.epifab.yadl.examples.TypesafeSchema.{Courses, Exams, Students, studentsSelect}
import io.epifab.yadl.typesafe._
import io.epifab.yadl.typesafe.fields.{AlwaysTrue, Column, Field}
import org.scalatest.{FlatSpec, Matchers}
import shapeless.{::, HNil}

class TypesafeTest extends FlatSpec with Matchers {
  "The TagMap typeclass" should "bind fields to their alias" in {
    val fields: Map[String, Field[Any]] =
      TagMap(studentsSelect.fields)

    fields.keys.toSet shouldBe Set("sid", "sname", "score", "cname")
  }

  "The source finder" should "get any source" in {
    val select: SelectContext[HNil, HNil, Table["students", _] with Tag["s"] :: Join[Table["exams", _] with Tag["e"]] :: Join[Table["courses", _] with Tag["c"]] :: HNil] = new SelectContext[HNil, HNil, Table["students", _] with Tag["s"] :: Join[Table["exams", _] with Tag["e"]] :: Join[Table["courses", _] with Tag["c"]] :: HNil] {
      override val placeholders: HNil = HNil
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

  "The QueryBuilder" should "build the fields clause" in {
    QueryBuilder[
      Column[Int] with Tag["sid"] :: Column[String] with Tag["sname"] :: Column[Option[Int]] with Tag["score"] :: Column[String] with Tag["cname"] :: HNil,
      "fields"
    ](studentsSelect.fields) shouldBe
      Some("s.id AS sid, s.name AS sname, ms.max_score AS score, cc.name AS cname")
  }
}
