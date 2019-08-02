package io.epifab.yadl

package object typesafe {
  type AS[T, A] = T with Alias[A]
}
