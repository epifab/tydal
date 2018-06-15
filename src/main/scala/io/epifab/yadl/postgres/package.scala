package io.epifab.yadl

import java.sql.Connection

import cats.Id
import io.epifab.yadl.domain.{QueryBuilder, QueryRunner, Statement}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

package object postgres {
  protected val queryBuilder: QueryBuilder[Statement] = PostgresQueryBuilder.build

  def asyncQueryRunner(connection: Connection)(implicit executionContext: ExecutionContext): QueryRunner[Future] =
    new AsyncPostgresQueryRunner(connection, queryBuilder)

  def syncQueryRunner(connection: Connection): QueryRunner[Id] =
    new PostgresQueryRunner(connection, queryBuilder)
}
