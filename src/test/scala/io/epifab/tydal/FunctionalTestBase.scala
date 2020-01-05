package io.epifab.tydal

import cats.effect.{Blocker, ContextShift, IO, Resource}
import io.epifab.tydal.runtime.{ConnectionPool, DataError, HikariConnectionPool, PostgresConfig, Transaction}

import scala.concurrent.ExecutionContext

trait FunctionalTestBase {

  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val connectionPool: ConnectionPool[IO] =
    ConnectionPool[IO](
      PostgresConfig.fromEnv("postgres://root:p4ssw0rd@localhost:5432/tydal"),
      ExecutionContext.global,
      ExecutionContext.global
    )

  implicit class ExtendedTransaction[C](transaction: Transaction[C]) {
    def runSync(): Either[DataError, C] =
      transaction.transact(connectionPool).unsafeRunSync()
  }

}
