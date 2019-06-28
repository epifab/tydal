package io.epifab.yadl.domain

package object typesafe {
  trait Alias[A]
  type AS[T, A] = T with Alias[A]

  trait Taggable {
    def as[A]: this.type with Alias[A] =
      this.asInstanceOf[this.type with Alias[A]]
  }
}
