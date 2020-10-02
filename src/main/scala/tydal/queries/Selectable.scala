package tydal.queries

import tydal.Tagging
import tydal.utils.TaggedFind
import shapeless.HList

trait FindContext[Haystack] {
  def apply[T <: String with Singleton, X](tag: T)(implicit find: TaggedFind[T, X, Haystack]): X with Tagging[T]
}

trait Selectable[Fields] extends FindContext[Fields] { self: Tagging[_] =>
  def fields: Fields
}

sealed trait FieldsOf[-S, +Fields <: HList] {
  def apply(selectable: S): Fields
}

object FieldsOf {
  implicit def selectable[Fields <: HList]: FieldsOf[Selectable[Fields], Fields] = new FieldsOf[Selectable[Fields], Fields] {
    override def apply(selectable: Selectable[Fields]): Fields = selectable.fields
  }
}
