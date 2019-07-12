package io.epifab.yadl.domain.typesafe

trait Alias[A]

trait Taggable {
  def as[A]: this.type with Alias[A] =
    this.asInstanceOf[this.type with Alias[A]]
}
