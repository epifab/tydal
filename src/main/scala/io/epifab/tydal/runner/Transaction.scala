package io.epifab.tydal.runner

import java.sql.Connection

import cats.Monad
import cats.effect.IO
import cats.implicits._
import io.epifab.tydal._
import io.epifab.tydal.runner.Transaction.{FlatMapTransaction, MapTransaction}
import shapeless.HList

import scala.concurrent.{ExecutionContext, Future}

trait Transaction[+Output] {
  def toIO(connection: Connection): IO[Either[DataError, Output]] =
    transact[IO](connection)

  def toFuture(connection: Connection)(implicit executionContext: ExecutionContext): Future[Either[DataError, Output]] =
    transact[Future](connection)

  def sync(connection: Connection): Either[DataError, Output] =
    transact[Covariant.Id](connection)

  final def transact[F[+_]: Eff: Monad](connection: Connection): F[Either[DataError, Output]] =
    ensureNonAutoCommit(connection).flatMap(_ =>
      Eff[F].delay(connection.setSavepoint()).flatMap(savePoint =>
        run(connection).flatMap {
          case Left(error) => Eff[F].delay(connection.rollback(savePoint)).map(_ => Left(error))
          case Right(results) => Eff[F].delay(connection.commit()).map(_ => Right(results))
        }
      )
    )

  private def ensureNonAutoCommit[F[+_]: Eff: Monad](connection: Connection): F[Unit] = for {
    autoCommit <- Eff[F].delay(connection.getAutoCommit)
    _ <- if (autoCommit) Eff[F].delay(connection.setAutoCommit(false)) else Eff[F].pure(())
  } yield ()

  protected def run[F[+_]: Eff: Monad](connection: Connection): F[Either[DataError, Output]]

  final def map[O2](f: Output => O2): Transaction[O2] =
    new MapTransaction(this, f)

  final def flatMap[O2](f: Output => Transaction[O2]): Transaction[O2] =
    new FlatMapTransaction(this, f)
}

object Transaction {
  class MapTransaction[O1, Output](transactionIO: Transaction[O1], f: O1 => Output) extends Transaction[Output] {
    override def run[F[+_]: Eff: Monad](connection: Connection): F[Either[DataError, Output]] =
      transactionIO.run(connection).map(_.map(f))
  }

  class FlatMapTransaction[O1, Output](transactionIO: Transaction[O1], f: O1 => Transaction[Output]) extends Transaction[Output] {
    override def run[F[+_]: Eff: Monad](connection: Connection): F[Either[DataError, Output]] =
      transactionIO.run(connection).flatMap {
        case Right(results) => f(results).run(connection)
        case Left(error) => Eff[F].pure(Left(error))
      }
  }

  def apply[Fields <: HList, Output]
  (runnableStatement: RunnableStatement[Fields])
  (implicit statementExecutor: StatementExecutor[Connection, Fields, Output]): Transaction[Output] = new Transaction[Output] {
    override protected def run[F[+_]: Eff: Monad](connection: Connection): F[Either[DataError, Output]] =
      statementExecutor.run(connection, runnableStatement)
  }
}
