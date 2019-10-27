package io.epifab.yadl.typesafe.runner

import java.sql.{Connection, PreparedStatement}

import cats.data.EitherT
import cats.effect.IO
import io.epifab.yadl.typesafe.{DataError, DriverError, IOEither}

import scala.util.Try
import scala.util.control.NonFatal

trait UpdateStatementExecutor[F[+_, +_], CONN] {
  def run(connection: CONN, statement: RunnableUpdateStatement): F[DataError, Int]
}

object UpdateStatementExecutor {
  implicit def jdbcQuery: UpdateStatementExecutor[IOEither, Connection] =

    new UpdateStatementExecutor[IOEither, Connection] {
      override def run(connection: Connection, statement: RunnableUpdateStatement): IO[Either[DataError, Int]] =
        (for {
          preparedStatement <- EitherT(IO(Jdbc.initStatement(connection, statement.sql, statement.input)))
          results <- EitherT(IO(runStatement(preparedStatement)))
        } yield results).value

      private def runStatement(preparedStatement: PreparedStatement): Either[DataError, Int] =
        Try(preparedStatement.executeUpdate()).toEither match {
          case Right(updatedRows) => Right(updatedRows)
          case Left(NonFatal(e)) => Left(DriverError(e.getMessage))
          case Left(fatalError) => throw fatalError
        }
    }
}
