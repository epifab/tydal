package io.epifab.yadl.examples

import java.sql.Connection

import cats.Id
import io.epifab.yadl.domain.QueryRunner
import io.epifab.yadl.{PostgresConfig, PostgresConnection}

import scala.concurrent.{ExecutionContext, Future}

object QueryRunnerFactories {
  lazy val connection: Connection = PostgresConnection(PostgresConfig.fromEnv())

  implicit def asyncQueryRunner(implicit executionContext: ExecutionContext): QueryRunner[Future] =
    io.epifab.yadl.postgres.asyncQueryRunner(connection, executionContext)

  implicit def syncQueryRunner: QueryRunner[Id] =
    io.epifab.yadl.postgres.syncQueryRunner(connection)
}
