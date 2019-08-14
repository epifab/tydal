package io.epifab.yadl

package object typesafe {
  type AS[+T, TAG] = T with Tag[TAG]
}
