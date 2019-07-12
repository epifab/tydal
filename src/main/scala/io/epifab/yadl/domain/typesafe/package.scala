package io.epifab.yadl.domain

package object typesafe {
  type AS[T, A] = T with Alias[A]
}
