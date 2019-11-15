package io.epifab.tydal.runner

import java.sql.{Connection, PreparedStatement, ResultSet}

import cats.Monad
import cats.data.EitherT
import cats.effect.{IO, Sync}
import io.epifab.tydal._
import shapeless.{HList, HNil}

import scala.util.Try
import scala.util.control.NonFatal

trait StatementExecutor[CONN, FIELDS <: HList, OUTPUT] {
  def run[F[_]: Sync: Monad](connection: CONN, statement: RunnableStatement[FIELDS]): F[Either[DataError, OUTPUT]]
}

trait ReadStatementExecutor[CONN, FIELDS <: HList, ROW]
  extends StatementExecutor[CONN, FIELDS, Iterator[Either[DecoderError, ROW]]]

trait WriteStatementExecutor[CONN, FIELDS <: HList]
  extends StatementExecutor[CONN, FIELDS, Int]


object ReadStatementExecutor {
  implicit def jdbcQuery[FIELDS <: HList, ROW]
  (implicit dataExtractor: DataExtractor[ResultSet, FIELDS, ROW]): ReadStatementExecutor[Connection, FIELDS, ROW] =

    new ReadStatementExecutor[Connection, FIELDS, ROW] {
      override def run[F[_]: Sync: Monad](connection: Connection, statement: RunnableStatement[FIELDS]): F[Either[DataError, Iterator[Either[DecoderError, ROW]]]] =
        (for {
          preparedStatement <- EitherT(Sync[F].delay(Jdbc.initStatement(connection, statement.sql, statement.input)))
          results <- EitherT(Sync[F].delay(runStatement(preparedStatement, statement.fields)))
        } yield results).value

      private def runStatement(preparedStatement: PreparedStatement, fields: FIELDS): Either[DataError, Iterator[Either[DecoderError, ROW]]] =
        Try(preparedStatement.executeQuery()).toEither match {
          case Right(resultSet) => Right(extract(fields, preparedStatement, resultSet))
          case Left(NonFatal(e)) => Left(DriverError(e.getMessage))
          case Left(fatalError) => throw fatalError
        }

      private def extract(fields: FIELDS, preparedStatement: PreparedStatement, resultSet: ResultSet): Iterator[Either[DecoderError, ROW]] = {
        new Iterator[Either[DecoderError, ROW]] {
          override def hasNext: Boolean = {
            if (!resultSet.next()) {
              try { resultSet.close(); preparedStatement.close(); false }
              catch { case NonFatal(_) => false }
            }
            else true
          }
          override def next(): Either[DecoderError, ROW] = dataExtractor.extract(resultSet, fields)
        }
      }
    }
}

object StatementExecutor {
  implicit def jdbcUpdate: WriteStatementExecutor[Connection, HNil] =
    new WriteStatementExecutor[Connection, HNil] {
      override def run[F[_]: Sync: Monad](connection: Connection, statement: RunnableStatement[HNil]): F[Either[DataError, Int]] =
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
