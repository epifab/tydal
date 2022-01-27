package tydal.runtime

import cats.effect.{Async, Resource}
import com.zaxxer.hikari.HikariDataSource

import java.sql.Connection
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

trait ConnectionPool[F[_]] {
  def connection: Resource[F, Connection]
}

case class PoolConfig(maxPoolSize: Int)

object PoolConfig {
  val default: PoolConfig = PoolConfig(10)
}

object ConnectionPool {
  def resource[F[_] : Async](postgresConfig: PostgresConfig, poolConfig: PoolConfig): Resource[F, ConnectionPool[F]] = {
    val connectionEC = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(poolConfig.maxPoolSize * 2))

    val dsAcquire = Async[F].blocking(createDataSource(postgresConfig, poolConfig))
    val dsRelease = (ds: HikariDataSource) => Async[F].blocking(ds.close())

    Resource.make(dsAcquire)(dsRelease).map(ds => new HikariConnectionPool(ds, connectionEC))
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

class HikariConnectionPool[M[_]: Async](
  dataSource: HikariDataSource,
  connectionEC: ExecutionContext
) extends ConnectionPool[M] {

  override val connection: Resource[M, Connection] = {
    val acquire = Async[M].evalOn(Async[M].delay(dataSource.getConnection), connectionEC)
    def release(c: Connection) = Async[M].blocking(c.close())
    Resource.make(acquire)(release)
  }

}
