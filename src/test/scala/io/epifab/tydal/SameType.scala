package io.epifab.tydal

trait SameType[T, U]

object SameType {
  implicit def trivial[T]: SameType[T, T] = new SameType[T, T] {}
}