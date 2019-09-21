package io.epifab.yadl.utils

import cats.Monad
import cats.implicits._

object MonadicOps {
  def sequentialFold[F[_]: Monad, T, U, V](zero: U, ts: Iterable[T], f: T => F[V], reduce: (U, V) => U): F[U] = {
    var result: F[U] = Monad[F].pure(zero)
    for (t <- ts) {
      result = result.flatMap(u => f(t).map(v => reduce(u, v)))
    }
    result
  }

  def sequentialFlatMap[F[_] : Monad, T, U](ts: Seq[T], f: T => F[Seq[U]]): F[Seq[U]] =
    sequentialFold(Seq.empty[U], ts, f, (u: Seq[U], v: Seq[U]) => u ++ v)

  def sequentialMap[F[_] : Monad, T, U](ts: Seq[T], f: T => F[U]): F[Seq[U]] =
    sequentialFold(Seq.empty[U], ts, f, (u: Seq[U], v: U) => u :+ v)

  implicit class ExtendedEitherSeq[E, R](list: Seq[Either[E, R]]) {
    def getOrFirstError: Either[E, List[R]] = {
      list match {
        case Right(r) :: tail =>
          tail.getOrFirstError match {
            case Right(rs) => Right(r :: rs)
            case Left(e) => Left(e)
          }

        case Left(e) :: _ =>
          Left(e)

        case Nil => Right(List.empty)
      }
    }
  }

  implicit class ExtendedEitherTSeq[F[_]: Monad, E, R](list: Seq[F[Either[E, R]]]) {
    def getOrFirstError: F[Either[E, Seq[R]]] =
      sequence(list).map(_.getOrFirstError)
  }

  def sequence[F[_]: Monad, T](list: Seq[F[T]]): F[Seq[T]] = list match {
    case fhead :: ftail =>
      for {
        head <- fhead
        tail <- sequence(ftail)
      } yield head +: tail
    case Nil => Monad[F].pure(Seq.empty)
  }
}
