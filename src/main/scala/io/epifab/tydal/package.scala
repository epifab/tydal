package io.epifab

import cats.effect.IO
import io.epifab.tydal.fields.{FieldDecoder, FieldEncoder, PlaceholderValue}

package object tydal {
  type DataError = runner.DataError

  type Tag = String with Singleton
  type AS[+T, A <: String with Singleton] = T with Tagging[A]
  type IOEither[+ERR, +OUT] = IO[Either[ERR, OUT]]

  implicit class ExtendedTag[A <: String with Singleton](tag: A)(implicit valueOf: ValueOf[A]) {
    def ~~>[T](value: T)(implicit encoder: FieldEncoder[T], decoder: FieldDecoder[T]): PlaceholderValue[T] with Tagging[A] = PlaceholderValue(value).as(tag)
  }
}
