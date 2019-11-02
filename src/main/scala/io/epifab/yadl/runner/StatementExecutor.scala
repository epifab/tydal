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

object StatementExecutor {
  implicit def jdbcQuery[FIELDS <: HList, OUTPUT]
    (implicit dataExtractor: DataExtractor[ResultSet, FIELDS, OUTPUT]): StatementExecutor[IOEither, Connection, FIELDS, Seq[OUTPUT]] =

    new StatementExecutor[IOEither, Connection, FIELDS, Seq[OUTPUT]] {
      override def run(connection: Connection, statement: RunnableStatement[FIELDS]): IO[Either[DataError, Seq[OUTPUT]]] =
        (for {
          preparedStatement <- EitherT(IO(Jdbc.initStatement(connection, statement.sql, statement.input)))
          results <- EitherT(IO(runStatement(preparedStatement, statement.fields)))
        } yield results).value

      private def runStatement(preparedStatement: PreparedStatement, fields: FIELDS): Either[DataError, Seq[OUTPUT]] =
        Try(preparedStatement.executeQuery()).toEither match {
          case Right(resultSet) => extract(fields, resultSet)
          case Left(NonFatal(e)) => Left(DriverError(e.getMessage))
          case Left(fatalError) => throw fatalError
        }

      private def extract(fields: FIELDS, resultSet: ResultSet): Either[DecoderError, Seq[OUTPUT]] = {
        import io.epifab.yadl.utils.MonadicOps._

        val iterator = new Iterator[Either[DecoderError, OUTPUT]] {
          override def hasNext: Boolean = resultSet.next()
          override def next(): Either[DecoderError, OUTPUT] = dataExtractor.extract(resultSet, fields)
        }

        iterator.toList.getOrFirstError
      }
    }

  implicit def jdbcUpdate: StatementExecutor[IOEither, Connection, HNil, Int] =

    new StatementExecutor[IOEither, Connection, HNil, Int] {
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
