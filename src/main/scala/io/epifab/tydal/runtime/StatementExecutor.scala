package io.epifab.tydal.runtime

import java.sql.{Connection, PreparedStatement, ResultSet}

import cats.Monad
import cats.data.EitherT
import cats.effect.Sync
import shapeless.{HList, HNil}

import scala.util.Try
import scala.util.control.NonFatal

trait StatementExecutor[CONN, Fields <: HList, Output] {
  def run[F[+_]: Sync : Monad](connection: CONN, statement: RunnableStatement[Fields]): F[Either[DataError, Output]]
}

trait ReadStatementExecutor[CONN, Fields <: HList, ROW]
  extends StatementExecutor[CONN, Fields, Iterator[Either[DecoderError, ROW]]]

trait WriteStatementExecutor[CONN, Fields <: HList]
  extends StatementExecutor[CONN, Fields, Int]


object ReadStatementExecutor {
  implicit def jdbcQuery[Fields <: HList, Row](
    implicit
    dataExtractor: DataExtractor[ResultSet, Fields, Row]
  ): ReadStatementExecutor[Connection, Fields, Row] =

    new ReadStatementExecutor[Connection, Fields, Row] {
      override def run[F[+_]: Sync : Monad](connection: Connection, statement: RunnableStatement[Fields]): F[Either[DataError, Iterator[Either[DecoderError, Row]]]] =
        (for {
          preparedStatement <- EitherT(Sync[F].delay(Jdbc.initStatement(connection, statement.sql, statement.input)))
          results <- EitherT(Sync[F].delay(runStatement(preparedStatement, statement.fields)))
        } yield results).value

      private def runStatement(preparedStatement: PreparedStatement, fields: Fields): Either[DataError, Iterator[Either[DecoderError, Row]]] =
        Try(preparedStatement.executeQuery()).toEither match {
          case Right(resultSet) => Right(extract(fields, preparedStatement, resultSet))
          case Left(NonFatal(e)) => Left(DriverError(e.getMessage))
          case Left(fatalError) => throw fatalError
        }

      private def extract(fields: Fields, preparedStatement: PreparedStatement, resultSet: ResultSet): Iterator[Either[DecoderError, Row]] = {
        new Iterator[Either[DecoderError, Row]] {
          override def hasNext: Boolean = {
            if (!resultSet.next()) {
              try { resultSet.close(); preparedStatement.close(); false }
              catch { case NonFatal(_) => false }
            }
            else true
          }
          override def next(): Either[DecoderError, Row] = dataExtractor.extract(resultSet, fields)
        }
      }
    }
}

object StatementExecutor {
  implicit def jdbcUpdate: WriteStatementExecutor[Connection, HNil] =
    new WriteStatementExecutor[Connection, HNil] {
      override def run[F[+_]: Sync : Monad](connection: Connection, statement: RunnableStatement[HNil]): F[Either[DataError, Int]] =
        (for {
          preparedStatement <- EitherT(Sync[F].delay(Jdbc.initStatement(connection, statement.sql, statement.input)))
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
