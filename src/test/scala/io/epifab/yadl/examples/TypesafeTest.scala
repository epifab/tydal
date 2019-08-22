package io.epifab.yadl.examples

import io.epifab.yadl.examples.TypesafeSchema.studentsSelect
import io.epifab.yadl.typesafe.{TagMap, Term}
import org.scalatest.{FlatSpec, Matchers}

class TypesafeTest extends FlatSpec with Matchers {
  "The TagMap typeclass" should "bind terms to their alias" in {
    val terms: Map[String, Term[Any]] =
      TagMap(studentsSelect.terms)

    terms.keys.toSet shouldBe Set("sid", "sname", "score")
  }
}
