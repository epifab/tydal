package io.epifab.yadl.typesafe

trait Tag[+A]

trait Taggable {
  def as[TAG]: this.type with Tag[TAG] =
    this.asInstanceOf[this.type with Tag[TAG]]
}
