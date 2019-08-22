package io.epifab.yadl.examples

import io.epifab.yadl.examples.TypesafeSchema.{Courses, Exams, Students, studentsSelect}
import io.epifab.yadl.typesafe._
import org.scalatest.{FlatSpec, Matchers}
import shapeless.{::, HNil}

class TypesafeTest extends FlatSpec with Matchers {
  "The TagMap typeclass" should "bind terms to their alias" in {
    val terms: Map[String, Term[Any]] =
      TagMap(studentsSelect.terms)

    terms.keys.toSet shouldBe Set("sid", "sname", "score", "cname")
  }

  "The source finder" should "get any source" in {
    val select: SelectContext[HNil, AS[Students, "s"] :: Join[AS[Exams, "e"]] :: Join[AS[Courses, "c"]] :: HNil] = new SelectContext[HNil, AS[Students, "s"] :: Join[AS[Exams, "e"]] :: Join[AS[Courses, "c"]] :: HNil] {
      override val sources: AS[Students, "s"] :: Join[AS[Exams, "e"]] :: Join[AS[Courses, "c"]] :: HNil =
        Students.as["s"] ::
          new Join(Exams.as["e"], AlwaysTrue) ::
          new Join(Courses.as["c"], AlwaysTrue) ::
          HNil

      override val placeholders: HNil = HNil
    }

    select.source["s"].get shouldBe a[Students]
    select.source["e"].get shouldBe a[Exams]
    select.source["c"].get shouldBe a[Courses]
  }
}
