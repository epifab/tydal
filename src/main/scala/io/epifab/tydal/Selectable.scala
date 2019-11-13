package io.epifab.tydal

import io.epifab.tydal.utils.TaggedFinder

trait FindContext[HAYSTACK] {
  def apply[T <: Tag, X](tag: T)(implicit finder: TaggedFinder[T, X, HAYSTACK]): X with Tagging[T]
}

trait Selectable[SCHEMA] extends FindContext[SCHEMA] { self: Tagging[_] =>
  def schema: SCHEMA
}

sealed trait SelectableSchema[-S <: Selectable[_], SCHEMA] {
  def schema(selectable: S): SCHEMA
}

object SelectableSchema {
  implicit def pure[SCHEMA]: SelectableSchema[Selectable[SCHEMA], SCHEMA] = new SelectableSchema[Selectable[SCHEMA], SCHEMA] {
    override def schema(selectable: Selectable[SCHEMA]): SCHEMA = selectable.schema
  }
}
