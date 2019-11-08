package io.epifab

import cats.effect.IO

package object tydal {
  type AS[+T, TAG <: String] = T with Tag[TAG]
  type IOEither[+ERR, +OUT] = IO[Either[ERR, OUT]]
}
