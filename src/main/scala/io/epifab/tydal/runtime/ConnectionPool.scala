package io.epifab.tydal.runtime

import java.sql.Connection

import cats.effect.{Async, ContextShift, Resource, Sync}
import com.zaxxer.hikari.HikariDataSource

import scala.concurrent.ExecutionContext

trait ConnectionPool[F[_]] {
  def connection: Resource[F, Connection]
}

object HikariConnectionPool {

  def apply[F[_] : Sync : Async : ContextShift](postgresConfig: PostgresConfig, connectionEC: ExecutionContext): Resource[F, ConnectionPool[F]] =
    for {
      _ <- Resource.liftF(Async[F].delay(Class.forName("org.postgresql.Driver")))
      dataSource <- createDataSourceResource(createDataSource(postgresConfig))
    } yield new HikariConnectionPool(dataSource, connectionEC)

  private def createDataSource(postgresConfig: PostgresConfig) = {
    val dataSource = new HikariDataSource
    dataSource setDriverClassName "org.postgresql.Driver"
    dataSource setJdbcUrl         postgresConfig.toUrl
    dataSource
  }

  private def createDataSourceResource[M[_] : Sync](factory: => HikariDataSource): Resource[M, HikariDataSource] = {
    val alloc = Sync[M].delay(factory)
    val free = (ds: HikariDataSource) => Sync[M].delay(ds.close())
    Resource.make(alloc)(free)
  }

}

class HikariConnectionPool[M[_] : Sync](
  dataSource: HikariDataSource,
  connectionEC: ExecutionContext)(
  implicit
  ev: Async[M],
  contextShift: ContextShift[M]
) extends ConnectionPool[M] {

  override def connection: Resource[M, Connection] = {
    val acquire = contextShift.evalOn(connectionEC)(ev.delay(dataSource.getConnection))
    def release(c: Connection) = ev.delay(c.close())
    Resource.make(acquire)(release)
  }

}
