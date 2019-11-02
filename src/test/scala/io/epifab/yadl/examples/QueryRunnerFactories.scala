package io.epifab.yadl.examples

import java.sql.Connection

import io.epifab.yadl.{PostgresConfig, PostgresConnection}

object QueryRunnerFactories {
  lazy val connection: Connection = PostgresConnection(PostgresConfig.fromEnv())
}
