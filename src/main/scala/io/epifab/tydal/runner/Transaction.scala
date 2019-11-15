package io.epifab.tydal.runner

import java.sql.Connection

import cats.{Functor, Id, Monad}
import cats.effect.{ExitCase, IO, Sync}
import cats.implicits._
import io.epifab.tydal._
import io.epifab.tydal.runner.Transaction.{FlatMapTransaction, MapTransaction}
import shapeless.ops.hlist.Tupler
import shapeless.{Generic, HList}

trait Transaction[+OUTPUT] {
  def toIO(connection: Connection): IO[Either[DataError, OUTPUT]] =
    transact[IO](connection)

  def sync(connection: Connection): Either[DataError, OUTPUT] =
    transact[IO](connection).unsafeRunSync()

  final def transact[F[+_]: Sync: Monad](connection: Connection): F[Either[DataError, OUTPUT]] =
    ensureNonAutoCommit(connection).flatMap(_ =>
      Sync[F].delay(connection.setSavepoint()).flatMap(savePoint =>
        run(connection).flatMap {
          case Left(error) => Sync[F].delay(connection.rollback(savePoint)).map(_ => Left(error))
          case Right(results) => Sync[F].delay(connection.commit()).map(_ => Right(results))
        }
      )
    )

  private def ensureNonAutoCommit[F[+_]: Sync: Monad](connection: Connection): F[Unit] = for {
    autoCommit <- Sync[F].delay(connection.getAutoCommit)
    _ <- if (autoCommit) Sync[F].delay(connection.setAutoCommit(false)) else Sync[F].pure(())
  } yield ()

  protected def run[F[+_]: Sync: Monad](connection: Connection): F[Either[DataError, OUTPUT]]

  final def map[O2](f: OUTPUT => O2): Transaction[O2] =
    new MapTransaction(this, f)

  final def flatMap[O2](f: OUTPUT => Transaction[O2]): Transaction[O2] =
    new FlatMapTransaction(this, f)
}

object Transaction {
  class MapTransaction[O1, OUTPUT](transactionIO: Transaction[O1], f: O1 => OUTPUT) extends Transaction[OUTPUT] {
    override def run[F[+_]: Sync: Monad](connection: Connection): F[Either[DataError, OUTPUT]] =
      transactionIO.run(connection).map(_.map(f))
  }

  class FlatMapTransaction[O1, OUTPUT](transactionIO: Transaction[O1], f: O1 => Transaction[OUTPUT]) extends Transaction[OUTPUT] {
    override def run[F[+_]: Sync: Monad](connection: Connection): F[Either[DataError, OUTPUT]] =
      transactionIO.run(connection).flatMap {
        case Right(results) => f(results).run(connection)
        case Left(error) => Sync[F].pure(Left(error))
      }
  }

  def apply[FIELDS <: HList, OUTPUT]
  (runnableStatement: RunnableStatement[FIELDS])
  (implicit statementExecutor: StatementExecutor[Connection, FIELDS, OUTPUT]): Transaction[OUTPUT] = new Transaction[OUTPUT] {
    override protected def run[F[+ _] : Sync : Monad](connection: Connection): F[Either[DataError, OUTPUT]] =
      statementExecutor.run(connection, runnableStatement)
  }
}
