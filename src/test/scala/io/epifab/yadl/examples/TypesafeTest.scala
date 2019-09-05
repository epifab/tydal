package io.epifab.yadl.examples

import io.epifab.yadl.examples.TypesafeSchema.{Courses, Exams, Students, studentsSelect}
import io.epifab.yadl.typesafe._
import io.epifab.yadl.typesafe.Implicits._
import io.epifab.yadl.typesafe.fields.{AlwaysTrue, Field}
import org.scalatest.{FlatSpec, Matchers}
import shapeless.{::, HNil}

class TypesafeTest extends FlatSpec with Matchers {
  "The TagMap typeclass" should "bind fields to their alias" in {
    val fields: Map[String, Field[Any]] =
      TagMap(studentsSelect.fields)

    fields.keys.toSet shouldBe Set("sid", "sname", "score", "cname")
  }

  "The source finder" should "get any source" in {
    val select: SelectContext[HNil, HNil, AS[Students, "s"] :: Join[AS[Exams, "e"]] :: Join[AS[Courses, "c"]] :: HNil] = new SelectContext[HNil, HNil, AS[Students, "s"] :: Join[AS[Exams, "e"]] :: Join[AS[Courses, "c"]] :: HNil] {
      override val placeholders: HNil = HNil
      override val fields: HNil = HNil
      override val sources: AS[Students, "s"] :: Join[AS[Exams, "e"]] :: Join[AS[Courses, "c"]] :: HNil =
        Students.as["s"] ::
          new Join(Exams.as["e"], AlwaysTrue) ::
          new Join(Courses.as["c"], AlwaysTrue) ::
          HNil
    }

    select["s"].get shouldBe a[Students]
    select["e"].get shouldBe a[Exams]
    select["c"].get shouldBe a[Courses]
  }
}
