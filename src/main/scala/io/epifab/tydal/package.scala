package io.epifab

import io.epifab.tydal.Tagging
import io.epifab.tydal.schema._

package object tydal {

  type Tag = String with Singleton

  type As[+T, A <: String with Singleton] = T with Tagging[A]

  type :=:[A <: String with Singleton, T] = Column[T] with Tagging[A]
  type ~~>[A <: String with Singleton, T] = TaggedLiteral[A, T]

  implicit class ExtendedTag[A <: String with Singleton](tag: A)(implicit valueOf: ValueOf[A]) {
    def ~~>[T](value: T): TaggedLiteral[A, T] = new TaggedLiteral(tag, value)
  }

  implicit class ExtendedFilterOption[+E <: Filter](val option: Option[E]) {
    def toFilter: FilterOption[E] = new FilterOption[E] {
      override def filter: Option[E] = option
    }
  }
}

class TaggedLiteral[A <: String with Singleton, +B](a: A, b: B) {
  def toLiteral[C >: B](implicit decoder: FieldDecoder[C], encoder: FieldEncoder[C]): Literal[C] with Tagging[A] =
    Literal[C](b).as(a)
}
