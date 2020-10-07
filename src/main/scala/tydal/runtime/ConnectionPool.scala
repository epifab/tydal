package tydal.runtime

import java.sql.Connection
import java.util.concurrent.Executors

import cats.effect.{Async, Blocker, ContextShift, Resource, Sync}
import com.zaxxer.hikari.HikariDataSource

import scala.concurrent.ExecutionContext

class JdbcExecutor(blocker: Blocker) {
  def apply[F[_] : Sync : ContextShift, A](f: => A): F[A] =
    blocker.delay(f)
}

trait ConnectionPool[F[_]] {
  def executor: JdbcExecutor
  def connection: Resource[F, Connection]
}

case class PoolConfig(maxPoolSize: Int)

object PoolConfig {
  val default: PoolConfig = PoolConfig(10)
}

object ConnectionPool {
  def resource[F[_] : Async : ContextShift](postgresConfig: PostgresConfig, poolConfig: PoolConfig): Resource[F, ConnectionPool[F]] = {
    val connectionEC = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(poolConfig.maxPoolSize * 2))

    for {
      blocker <- Blocker[F]
      dsAcquire = blocker.blockOn(Async[F].delay(createDataSource(postgresConfig, poolConfig)))
      dsRelease = (ds: HikariDataSource) => blocker.blockOn(Async[F].delay(ds.close()))
      dataSource <- Resource.make(dsAcquire)(dsRelease)
    } yield new HikariConnectionPool(dataSource, connectionEC, blocker)
  }

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
  blocker: Blocker
)(
  implicit
  contextShift: ContextShift[M]
) extends ConnectionPool[M] {

  override val executor: JdbcExecutor = new JdbcExecutor(blocker)

  override val connection: Resource[M, Connection] = {
    val acquire = contextShift.evalOn(connectionEC)(Sync[M].delay(dataSource.getConnection))
    def release(c: Connection) = blocker.delay(c.close())
    Resource.make(acquire)(release)
  }

}
