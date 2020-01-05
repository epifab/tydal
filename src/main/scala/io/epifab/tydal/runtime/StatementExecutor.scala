package io.epifab.tydal.runtime

import java.sql.{Connection, PreparedStatement}

import cats.Monad
import cats.data.EitherT
import cats.effect.{Blocker, ContextShift, Sync}
import shapeless.{HList, HNil}

import scala.util.Try
import scala.util.control.NonFatal

trait StatementExecutor[Conn, Fields <: HList, +Output] {
  def run[F[+_]: Sync : Monad : ContextShift](connection: Conn, blocker: Blocker, statement: RunnableStatement[Fields]): F[Either[DataError, Output]]
}

trait ReadStatementExecutor[Conn, Fields <: HList, Row, +C[_] <: Iterable[_]]
  extends StatementExecutor[Conn, Fields, C[Row]]

trait WriteStatementExecutor[Conn, Fields <: HList]
  extends StatementExecutor[Conn, Fields, Int]

object StatementExecutor {
  implicit val jdbcUpdate: WriteStatementExecutor[Connection, HNil] =
    new WriteStatementExecutor[Connection, HNil] {
      override def run[F[+_] : Sync : Monad : ContextShift](connection: Connection, blocker: Blocker, statement: RunnableStatement[HNil]): F[Either[DataError, Int]] =
        (for {
          preparedStatement <- EitherT(blocker.delay(Jdbc.initStatement(connection, statement.sql, statement.input)))
          results <- EitherT(Sync[F].delay(runStatement(preparedStatement)))
        } yield results).value

      private def runStatement(preparedStatement: PreparedStatement): Either[DataError, Int] =
        Try(preparedStatement.executeUpdate()).toEither match {
          case Right(updatedRows) => Right(updatedRows)
          case Left(NonFatal(e)) => Left(DriverError(e.getMessage))
          case Left(fatalError) => throw fatalError
        }
    }
}
