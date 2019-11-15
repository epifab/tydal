package io.epifab.tydal

import cats.effect.IO
import io.epifab.tydal.Covariant.Id

import scala.concurrent.{ExecutionContext, Future}

/**
 * Simpler version of cats Sync that can be extended to Future or even Id
 * @tparam F IO, Future, ...
 */
trait Eff[F[+_]] {
  def delay[A](x: => A): F[A]
  def pure[A](x: A): F[A]
}

object Covariant {
  type Id[+A] = A
}

object Eff {
  def apply[F[+_]](implicit eff: Eff[F]): Eff[F] = eff

  implicit def io: Eff[IO] = new Eff[IO] {
    override def delay[A](x: => A): IO[A] = IO.delay(x)
    override def pure[A](x: A): IO[A] = IO.pure(x)
  }

  implicit def dummy: Eff[Id] = new Eff[Id] {
    override def delay[A](x: => A): Id[A] = x
    override def pure[A](x: A): Id[A] = x
  }

  implicit def future(implicit executionContext: ExecutionContext): Eff[Future] = new Eff[Future] {
    override def delay[A](x: => A): Future[A] = Future(x)
    override def pure[A](x: A): Future[A] = Future.successful(x)
  }
}
