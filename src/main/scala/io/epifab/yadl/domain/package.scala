package io.epifab.yadl

package object domain {
  type Extractor[T] = Row => Either[ExtractorError, T]

  type QueryBuilder[T] = T => Query
}
