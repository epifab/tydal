package tydal

import cats.effect.{ContextShift, IO}
import tydal.runtime.{ConnectionPool, PoolConfig, PostgresConfig, Transaction}
import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.concurrent.ExecutionContext

trait FunctionalTestBase extends BeforeAndAfterAll { this: Suite =>

  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

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
