package io.epifab.yadl.domain

trait Extractor[T] { outer =>
  def extract(row: Row): Either[ExtractorError, T]

  def map[U](f: T => U): Extractor[U] =
    (row: Row) => outer.extract(row).map(f)
}
