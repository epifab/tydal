package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields.BinaryExpr
import io.epifab.yadl.typesafe.utils.TaggedFinder


trait FindContext[HAYSTACK] {
  def apply[TAG <: String with Singleton, X](tag: TAG)(implicit finder: TaggedFinder[TAG, X, HAYSTACK]): X with Tag[TAG]
}

trait Selectable[FIELDS] extends FindContext[FIELDS] { self: Tag[_] =>
  def fields: FIELDS

  def on[E <: BinaryExpr](clause: this.type => E): Join[this.type, E] =
    new Join(this, clause(this))
}
