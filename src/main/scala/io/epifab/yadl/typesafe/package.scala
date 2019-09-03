package io.epifab.yadl

package object typesafe {
  type AS[+T, TAG <: String] = T with Tag[TAG]
}
