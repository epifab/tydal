package io.epifab.tydal.examples

import java.sql.Connection

import io.epifab.tydal.runtime.{PostgresConfig, PostgresConnection}

object QueryRunnerFactories {
  lazy val connection: Connection = PostgresConnection(PostgresConfig.fromEnv())
}
