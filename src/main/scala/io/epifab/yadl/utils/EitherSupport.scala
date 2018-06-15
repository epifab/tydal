package io.epifab.yadl.utils

object EitherSupport {
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
