package io.epifab.yadl.domain

import scala.language.higherKinds

trait Row {
  def get[T](column: Field[T]): Either[ExtractorError, T]
}

trait QueryRunner[F[_]] {
  def run[T](query: Select[T]): F[Either[DALError, Seq[T]]]
  def run(query: Statement with SideEffect): F[Either[DALError, Int]]
}
