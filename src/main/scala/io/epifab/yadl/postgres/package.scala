package io.epifab.yadl

import java.sql.Connection

import cats.Id
import io.epifab.yadl.domain.{QueryBuilder, QueryRunner, Statement}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

package object postgres {
  protected val queryBuilder: QueryBuilder[Statement] = PostgresQueryBuilder.build

  implicit def asyncQueryRunner(implicit connection: Connection, executionContext: ExecutionContext): QueryRunner[Future] =
    new AsyncPostgresQueryRunner(connection, queryBuilder)

  implicit def syncQueryRunner(implicit connection: Connection): QueryRunner[Id] =
    new SyncQueryRunner(connection, queryBuilder)
}
