package io.epifab.tydal

import io.epifab.tydal.fields._

import scala.language.implicitConversions

object Implicits {
  implicit def tableToSchema[NAME <: Tag, SCHEMA](table: Table[_, SCHEMA]): SCHEMA = table.schema

  implicit class ExtendedTag[A <: Tag](tag: A)(implicit valueOf: ValueOf[A]) {
    def ~~>[T](value: T)(implicit encoder: FieldEncoder[T], decoder: FieldDecoder[T]): PlaceholderValue[T] with Tagging[A] = PlaceholderValue(value).as(tag)
    def ?[T](implicit fieldEncoder: FieldEncoder[T], fieldDecoder: FieldDecoder[T]): NamedPlaceholder[T] with Tagging[A] = NamedPlaceholder[T, A]
  }
}
