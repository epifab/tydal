package io.epifab.tydal.runtime

import cats.effect.Sync
import io.epifab.tydal.runtime.Covariant.Id

import scala.concurrent.{ExecutionContext, Future}

/**
 * Reduced version of cats Sync
 * @tparam F IO, Future (!!), ...
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

  implicit def sync[F[+_]: Sync]: Eff[F] = new Eff[F] {
    override def delay[A](x: => A): F[A] = Sync[F].delay(x)
    override def pure[A](x: A): F[A] = Sync[F].pure(x)
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
