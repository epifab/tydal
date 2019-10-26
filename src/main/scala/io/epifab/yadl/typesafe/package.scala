package io.epifab.yadl

import cats.effect.IO

package object typesafe {
  type AS[+T, TAG <: String] = T with Tag[TAG]
  type IOEither[+ERR, +OUT] = IO[Either[ERR, OUT]]
}
