package io.epifab.tydal

import io.epifab.tydal.utils.TaggedFinder
import shapeless.{Generic, HList}

trait FindContext[Haystack] {
  def apply[T <: String with Singleton, X](tag: T)(implicit finder: TaggedFinder[T, X, Haystack]): X with Tagging[T]
}

trait Selectable[Schema] extends FindContext[Schema] { self: Tagging[_] =>
  def rightFields: Schema
}

sealed trait SelectableFields[-S <: Selectable[_], Fields <: HList] {
  def fields(selectable: S): Fields
}

object SelectableFields {
  implicit def table[Fields <: HList]: SelectableFields[Table[_, Fields], Fields] = new SelectableFields[Table[_, Fields], Fields] {
    override def fields(selectable: Table[_, Fields]): Fields = selectable.rightFields
  }

  implicit def subQuery[Fields <: HList]: SelectableFields[SelectSubQuery[Fields, _], Fields] = new SelectableFields[SelectSubQuery[Fields, _], Fields] {
    override def fields(selectable: SelectSubQuery[Fields, _]): Fields = selectable.rightFields
  }
}
