package io.epifab.yadl

package object domain {
  type QueryBuilder[T] = T => Query

  case class Geometry(value: String) extends AnyVal
  case class Geography(value: String) extends AnyVal
}
