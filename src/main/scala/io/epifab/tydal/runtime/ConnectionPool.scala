package io.epifab.tydal.runtime

import java.sql.Connection
import java.util.concurrent.Executors

import cats.effect.{Async, Blocker, ContextShift, Resource, Sync}
import com.zaxxer.hikari.HikariDataSource

import scala.concurrent.ExecutionContext
import scala.util.Try

class JdbcExecutor(blocker: Blocker) {
  def apply[F[_] : Sync : ContextShift, A](f: => A): F[A] =
    blocker.delay(f)
}

trait ConnectionPool[F[_]] {
  def executor: JdbcExecutor
  def connection: Resource[F, Connection]
  def shutDown(): F[Unit]
}

case class PoolConfig(maxPoolSize: Int)

object PoolConfig {
  val default: PoolConfig = PoolConfig(10)
}

object ConnectionPool {
  def resource[F[_] : Async : ContextShift](postgresConfig: PostgresConfig, poolConfig: PoolConfig): Resource[F, ConnectionPool[F]] = {
    val connectionEC = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(poolConfig.maxPoolSize * 2))
    val blockingEC = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(poolConfig.maxPoolSize * 4))

    val acquire = Sync[F].delay(ConnectionPool(postgresConfig, connectionEC, blockingEC, poolConfig))
    val release: ConnectionPool[F] => F[Unit] = _.shutDown()

    Resource.make(acquire)(release)
  }

  private def apply[F[_] : Sync : Async : ContextShift](
    postgresConfig: PostgresConfig,
    connectionEC: ExecutionContext,
    blockingEC: ExecutionContext,
    poolConfig: PoolConfig = PoolConfig.default
  ): ConnectionPool[F] = new HikariConnectionPool(createDataSource(postgresConfig, poolConfig), connectionEC, blockingEC)

  private def createDataSource(postgresConfig: PostgresConfig, poolConfig: PoolConfig) = {
    val dataSource = new HikariDataSource
    dataSource.setDriverClassName("org.postgresql.Driver")
    dataSource.setJdbcUrl(postgresConfig.toUrl)
    dataSource.setAutoCommit(false)
    dataSource.setMaximumPoolSize(poolConfig.maxPoolSize)
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

  override def shutDown(): M[Unit] = Sync[M].delay(dataSource.close())

}
