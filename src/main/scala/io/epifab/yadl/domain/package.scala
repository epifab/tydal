package io.epifab.yadl

package object domain {
  type QueryBuilder[T] = T => Query
}
