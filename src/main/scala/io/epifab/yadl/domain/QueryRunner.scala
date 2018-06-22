package io.epifab.yadl.domain

import scala.language.higherKinds

class Col[T, U](field: Field[T], result: T)

trait Row {
  def get[T](field: Field[T]): Either[ExtractorError, T]
}

trait QueryRunner[F[_]] {
  def run[T](query: Select)(implicit extractor: Extractor[T]): F[Either[DALError, Seq[T]]]
  def run(query: Statement with SideEffect): F[Either[DALError, Int]]
}
