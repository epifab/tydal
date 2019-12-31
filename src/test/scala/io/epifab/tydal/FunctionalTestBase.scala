package io.epifab.tydal

import cats.effect.{Blocker, ContextShift, IO, Resource}
import io.epifab.tydal.runtime.{ConnectionPool, DataError, HikariConnectionPool, PostgresConfig, Transaction}

import scala.concurrent.ExecutionContext

trait FunctionalTestBase {

  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val connectionPoolResource: Resource[IO, ConnectionPool[IO]] =
    HikariConnectionPool[IO](
      PostgresConfig.fromEnv(),
      ExecutionContext.global
    )

  implicit class ExtendedTransaction[C](transaction: Transaction[C]) {
    def runSync(): Either[DataError, C] =
      connectionPoolResource.use(pool => transaction.transact(pool)).unsafeRunSync()
  }

}
