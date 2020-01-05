package io.epifab.tydal.runtime

import java.sql.Connection

import cats.effect.{Async, Blocker, ContextShift, Resource, Sync}
import com.zaxxer.hikari.HikariDataSource

import scala.concurrent.ExecutionContext
import scala.util.Try

class JdbcExecutor(blocker: Blocker) {
  def safe[F[_] : Sync : ContextShift, A](f: => A): F[Either[DataError, A]] =
    blocker.delay(Try(f).toEither.left.map(error => DriverError(error.getMessage)))

  def unsafe[F[_] : Sync : ContextShift, A](f: => A): F[A] =
    blocker.delay(f)
}

trait ConnectionPool[F[_]] {
  def executor: JdbcExecutor
  def connection: Resource[F, Connection]
}

object ConnectionPool {
  def apply[F[_] : Sync : Async : ContextShift](postgresConfig: PostgresConfig, connectionEC: ExecutionContext, blockingEC: ExecutionContext): ConnectionPool[F] =
    new HikariConnectionPool(createDataSource(postgresConfig), connectionEC, blockingEC)

  private def createDataSource(postgresConfig: PostgresConfig) = {
    val dataSource = new HikariDataSource
    dataSource.setDriverClassName("org.postgresql.Driver")
    dataSource.setJdbcUrl(postgresConfig.toUrl)
    dataSource
  }
}

class HikariConnectionPool[M[_] : Sync](
  dataSource: HikariDataSource,
  connectionEC: ExecutionContext,
  blockingEC: ExecutionContext)(
  implicit
  ev: Async[M],
  contextShift: ContextShift[M]
) extends ConnectionPool[M] {

  override val executor: JdbcExecutor = new JdbcExecutor(Blocker.liftExecutionContext(blockingEC))

  override val connection: Resource[M, Connection] = {
    val acquire = contextShift.evalOn(connectionEC)(ev.delay(dataSource.getConnection))
    def release(c: Connection) = ev.delay(c.close())
    Resource.make(acquire)(release)
  }

}
