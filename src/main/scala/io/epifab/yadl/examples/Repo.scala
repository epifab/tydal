package io.epifab.yadl.examples

import cats.Applicative
import io.epifab.yadl.domain.QueryRunner

import scala.language.higherKinds

trait Repo[F[_]] {
  implicit val queryRunner: QueryRunner[F]
  implicit val A: Applicative[F]
}
