package io.epifab.tydal.utils

import scala.collection.{Factory, mutable}

object EitherSupport {
  def leftOrRights[E, X, To[_]](from: Iterator[Either[E, X]])(implicit factory: Factory[X, To[X]]): Either[E, To[X]] = {
    val results = from.iterator.foldLeft[Either[E, mutable.Builder[X, To[X]]]](Right(factory.newBuilder)) {
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)
      case (Right(builder), Right(x)) => Right(builder.addOne(x))
    }
    results.map(_.result)
  }

  def firstLeftOrRights[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] =
    s.foldLeft[Either[A, Seq[B]]](Right(Seq.empty[B]))(
      (a: Either[A, Seq[B]], b: Either[A, B]) => {
        a.flatMap(results =>
          b match {
            case Left(error) =>
              // New error
              Left(error)
            case Right(result) =>
              Right(results :+ result)
          }
        )
      }
    )
}
