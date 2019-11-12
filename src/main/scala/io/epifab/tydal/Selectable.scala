package io.epifab.tydal

import io.epifab.tydal.utils.TaggedFinder

trait FindContext[HAYSTACK] {
  def apply[TAG <: String with Singleton, X](tag: TAG)(implicit finder: TaggedFinder[TAG, X, HAYSTACK]): X with Tag[TAG]
}

trait Selectable[SCHEMA] extends FindContext[SCHEMA] { self: Tag[_] =>
  def schema: SCHEMA
}
