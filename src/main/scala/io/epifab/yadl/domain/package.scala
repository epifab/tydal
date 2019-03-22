package io.epifab.yadl

package object domain {
  type QueryBuilder[T] = T => Query

  case class Point(value: String) extends AnyVal
}
