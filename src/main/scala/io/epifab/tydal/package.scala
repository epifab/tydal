package io.epifab

import io.epifab.tydal.schema._

package object tydal {

  type Tag = String with Singleton

  type As[+T, A <: String with Singleton] = T with Tagging[A]

  type :=:[A <: String with Singleton, T] = Column[T] with Tagging[A]
  type ~~>[A <: String with Singleton, T] = Literal[T] with Tagging[A]

  implicit class ExtendedTag[A <: String with Singleton](tag: A)(implicit valueOf: ValueOf[A]) {
    def ~~>[T](value: T)(implicit encoder: FieldEncoder[T], decoder: FieldDecoder[T]): Literal[T] with Tagging[A] = Literal(value).as(tag)
  }

  implicit class ExtendedFilterOption[+E <: Filter](val option: Option[E]) {
    def toFilter: FilterOption[E] = new FilterOption[E] {
      override def filter: Option[E] = option
    }
  }
}
