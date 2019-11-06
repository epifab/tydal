package io.epifab.yadl.runner

import java.sql.{Connection, PreparedStatement, ResultSet}

import cats.data.EitherT
import cats.effect.IO
import io.epifab.yadl._
import shapeless.{HList, HNil}

import scala.util.Try
import scala.util.control.NonFatal

trait StatementExecutor[F[+_, +_], CONN, FIELDS <: HList, OUTPUT] {
  def run(connection: CONN, statement: RunnableStatement[FIELDS]): F[DataError, OUTPUT]
}

trait ReadStatementExecutor[F[+_, +_], CONN, FIELDS <: HList, ROW]
  extends StatementExecutor[F, CONN, FIELDS, Iterator[Either[DecoderError, ROW]]]

trait WriteStatementExecutor[F[+_, +_], CONN, FIELDS <: HList]
  extends StatementExecutor[F, CONN, FIELDS, Int]


object ReadStatementExecutor {
  implicit def jdbcQuery[FIELDS <: HList, ROW]
  (implicit dataExtractor: DataExtractor[ResultSet, FIELDS, ROW]): ReadStatementExecutor[IOEither, Connection, FIELDS, ROW] =

    new ReadStatementExecutor[IOEither, Connection, FIELDS, ROW] {
      override def run(connection: Connection, statement: RunnableStatement[FIELDS]): IO[Either[DataError, Iterator[Either[DecoderError, ROW]]]] =
        (for {
          preparedStatement <- EitherT(IO(Jdbc.initStatement(connection, statement.sql, statement.input)))
          results <- EitherT(IO(runStatement(preparedStatement, statement.fields)))
        } yield results).value

      private def runStatement(preparedStatement: PreparedStatement, fields: FIELDS): Either[DataError, Iterator[Either[DecoderError, ROW]]] =
        Try(preparedStatement.executeQuery()).toEither match {
          case Right(resultSet) => Right(extract(fields, resultSet))
          case Left(NonFatal(e)) => Left(DriverError(e.getMessage))
          case Left(fatalError) => throw fatalError
        }

      private def extract(fields: FIELDS, resultSet: ResultSet): Iterator[Either[DecoderError, ROW]] = {
        new Iterator[Either[DecoderError, ROW]] {
          override def hasNext: Boolean = resultSet.next()
          override def next(): Either[DecoderError, ROW] = dataExtractor.extract(resultSet, fields)
        }
      }
    }
}

object StatementExecutor {
  implicit def jdbcUpdate: WriteStatementExecutor[IOEither, Connection, HNil] =
    new WriteStatementExecutor[IOEither, Connection, HNil] {
      override def run(connection: Connection, statement: RunnableStatement[HNil]): IO[Either[DataError, Int]] =
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
