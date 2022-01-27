package tydal.runtime

import cats.effect.Async
import cats.implicits._
import shapeless.{HList, HNil}

import java.sql.{Connection, PreparedStatement}

trait StatementExecutor[Conn, Fields <: HList, +Output] {
  def run[F[+_]: Async](connection: Conn, statement: RunnableStatement[Fields]): F[Output]
}

trait ReadStatementExecutor[Conn, Fields <: HList, Row, +C[_] <: Iterable[_]]
  extends StatementExecutor[Conn, Fields, C[Row]]

trait WriteStatementExecutor[Conn, Fields <: HList]
  extends StatementExecutor[Conn, Fields, Int]

object StatementExecutor {
  implicit val jdbcUpdate: WriteStatementExecutor[Connection, HNil] =
    new WriteStatementExecutor[Connection, HNil] {
      override def run[F[+_]: Async](connection: Connection, statement: RunnableStatement[HNil]): F[Int] =
        for {
          preparedStatement <- Jdbc.initStatement(connection, statement.sql, statement.input)
          results <- Async[F].delay(unsafeRunStatement(preparedStatement))
        } yield results

      private def unsafeRunStatement(preparedStatement: PreparedStatement): Int =
        preparedStatement.executeUpdate()
    }
}
