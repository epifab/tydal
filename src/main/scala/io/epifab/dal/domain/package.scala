package io.epifab.dal

package object domain {
  type Extractor[T] = Row => Either[ExtractorError, T]
}
