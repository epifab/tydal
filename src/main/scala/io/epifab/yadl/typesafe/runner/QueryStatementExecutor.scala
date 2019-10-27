package io.epifab.yadl.typesafe.runner

import java.sql.{Connection, PreparedStatement, ResultSet}

import cats.data.EitherT
import cats.effect.IO
import io.epifab.yadl.typesafe.{DataError, DecoderError, DriverError, IOEither}
import shapeless.{HList, HNil}

import scala.util.Try
import scala.util.control.NonFatal

trait QueryStatementExecutor[F[+_, +_], CONN, FIELDS <: HList, OUTPUT] {
  def run(connection: CONN, statement: RunnableQueryStatement[FIELDS]): F[DataError, OUTPUT]
}

object QueryStatementExecutor {
  implicit def jdbcQuery[FIELDS <: HList, OUTPUT]
    (implicit dataExtractor: DataExtractor[ResultSet, FIELDS, OUTPUT]): QueryStatementExecutor[IOEither, Connection, FIELDS, Seq[OUTPUT]] =

    new QueryStatementExecutor[IOEither, Connection, FIELDS, Seq[OUTPUT]] {
      override def run(connection: Connection, statement: RunnableQueryStatement[FIELDS]): IO[Either[DataError, Seq[OUTPUT]]] =
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
}
