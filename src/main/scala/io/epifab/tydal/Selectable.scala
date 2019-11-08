package io.epifab.tydal

import io.epifab.tydal.fields.BinaryExpr
import io.epifab.tydal.utils.TaggedFinder


trait FindContext[HAYSTACK] {
  def apply[TAG <: String with Singleton, X](tag: TAG)(implicit finder: TaggedFinder[TAG, X, HAYSTACK]): X with Tag[TAG]
}

trait Selectable[FIELDS] extends FindContext[FIELDS] { self: Tag[_] =>
  def schema: FIELDS

  def on[E <: BinaryExpr](clause: this.type => E): Join[this.type, E] =
    new Join(this, clause(this))
}
