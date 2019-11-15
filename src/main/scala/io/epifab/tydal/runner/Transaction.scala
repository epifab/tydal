package io.epifab.tydal.runner

import java.sql.Connection

import cats.Monad
import cats.effect.IO
import cats.implicits._
import io.epifab.tydal._
import io.epifab.tydal.runner.Transaction.{FlatMapTransaction, MapTransaction}
import shapeless.HList

import scala.concurrent.{ExecutionContext, Future}

trait Transaction[+OUTPUT] {
  def toIO(connection: Connection): IO[Either[DataError, OUTPUT]] =
    transact[IO](connection)

  def toFuture(connection: Connection)(implicit executionContext: ExecutionContext): Future[Either[DataError, OUTPUT]] =
    transact[Future](connection)

  def sync(connection: Connection): Either[DataError, OUTPUT] =
    transact[Covariant.Id](connection)

  final def transact[F[+_]: Eff: Monad](connection: Connection): F[Either[DataError, OUTPUT]] =
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

  protected def run[F[+_]: Eff: Monad](connection: Connection): F[Either[DataError, OUTPUT]]

  final def map[O2](f: OUTPUT => O2): Transaction[O2] =
    new MapTransaction(this, f)

  final def flatMap[O2](f: OUTPUT => Transaction[O2]): Transaction[O2] =
    new FlatMapTransaction(this, f)
}

object Transaction {
  class MapTransaction[O1, OUTPUT](transactionIO: Transaction[O1], f: O1 => OUTPUT) extends Transaction[OUTPUT] {
    override def run[F[+_]: Eff: Monad](connection: Connection): F[Either[DataError, OUTPUT]] =
      transactionIO.run(connection).map(_.map(f))
  }

  class FlatMapTransaction[O1, OUTPUT](transactionIO: Transaction[O1], f: O1 => Transaction[OUTPUT]) extends Transaction[OUTPUT] {
    override def run[F[+_]: Eff: Monad](connection: Connection): F[Either[DataError, OUTPUT]] =
      transactionIO.run(connection).flatMap {
        case Right(results) => f(results).run(connection)
        case Left(error) => Eff[F].pure(Left(error))
      }
  }

  def apply[FIELDS <: HList, OUTPUT]
  (runnableStatement: RunnableStatement[FIELDS])
  (implicit statementExecutor: StatementExecutor[Connection, FIELDS, OUTPUT]): Transaction[OUTPUT] = new Transaction[OUTPUT] {
    override protected def run[F[+_]: Eff: Monad](connection: Connection): F[Either[DataError, OUTPUT]] =
      statementExecutor.run(connection, runnableStatement)
  }
}
