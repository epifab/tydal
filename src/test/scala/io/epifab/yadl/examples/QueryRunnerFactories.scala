package io.epifab.yadl.examples

import java.sql.{Connection, DriverManager}

import cats.Id
import io.epifab.yadl.domain.QueryRunner

import scala.concurrent.{ExecutionContext, Future}

object QueryRunnerFactories {
  lazy val connection: Connection =
    DriverManager.getConnection(
      s"jdbc:postgresql://%s:%s/%s?user=%s&password=%s&sslmode=require".format(
        sys.env("DB_HOST"),
        sys.env("DB_PORT"),
        sys.env("DB_NAME"),
        sys.env("DB_USER"),
        sys.env("DB_PASS")
      )
    )

  implicit def asyncQueryRunner(implicit executionContext: ExecutionContext): QueryRunner[Future] =
    io.epifab.yadl.postgres.asyncQueryRunner(connection)

  implicit def syncQueryRunner: QueryRunner[Id] =
    io.epifab.yadl.postgres.syncQueryRunner(connection)
}
