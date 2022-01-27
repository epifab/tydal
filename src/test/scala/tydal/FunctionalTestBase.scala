package tydal

import cats.effect.IO
import org.scalatest.{BeforeAndAfterAll, Suite}
import tydal.runtime.{ConnectionPool, PoolConfig, PostgresConfig, Transaction}
import cats.effect.unsafe.implicits.global

trait FunctionalTestBase extends BeforeAndAfterAll { this: Suite =>

  val (connectionPool, connectionPoolShutDown) =
    ConnectionPool.resource[IO](
      PostgresConfig.fromEnv("postgres://root:p4ssw0rd@localhost:5432/tydal"),
      PoolConfig(2)
    ).allocated.unsafeRunSync()

  override def afterAll(): Unit = {
    super.afterAll()
    connectionPoolShutDown.unsafeRunSync()
  }

  implicit class ExtendedTransaction[C](transaction: Transaction[C]) {
    def unsafeRun(): C = transaction.transact(connectionPool).unsafeRunSync()
    def run(): Either[Throwable, C] = transaction.transact(connectionPool).attempt.unsafeRunSync()
  }

}
