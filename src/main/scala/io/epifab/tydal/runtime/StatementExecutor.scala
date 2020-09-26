package io.epifab.tydal.runtime

import java.sql.{Connection, PreparedStatement}

import cats.effect.{ContextShift, Sync}
import cats.implicits._
import shapeless.{HList, HNil}

trait StatementExecutor[Conn, Fields <: HList, +Output] {
  def run[F[+_] : Sync : ContextShift](connection: Conn, jdbcExecutor: JdbcExecutor, statement: RunnableStatement[Fields]): F[Output]
}

trait ReadStatementExecutor[Conn, Fields <: HList, Row, +C[_] <: Iterable[_]]
  extends StatementExecutor[Conn, Fields, C[Row]]

trait WriteStatementExecutor[Conn, Fields <: HList]
  extends StatementExecutor[Conn, Fields, Int]

object StatementExecutor {
  implicit val jdbcUpdate: WriteStatementExecutor[Connection, HNil] =
    new WriteStatementExecutor[Connection, HNil] {
      override def run[F[+_] : Sync : ContextShift](connection: Connection, jdbcExecutor: JdbcExecutor, statement: RunnableStatement[HNil]): F[Int] =
        for {
          preparedStatement <- Jdbc.initStatement(connection, jdbcExecutor, statement.sql, statement.input)
          results <- Sync[F].delay(unsafeRunStatement(preparedStatement))
        } yield results

      private def unsafeRunStatement(preparedStatement: PreparedStatement): Int =
        preparedStatement.executeUpdate()
    }
}
