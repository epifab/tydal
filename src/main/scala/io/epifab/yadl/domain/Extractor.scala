package io.epifab.yadl.domain

trait Row {
  def get[T](term: Term[T]): Either[ExtractorError, T]
}

trait Extractor[T] { outer =>
  def extract(row: Row): Either[ExtractorError, T]

  def map[U](f: T => U): Extractor[U] =
    (row: Row) => outer.extract(row).map(f)
}
