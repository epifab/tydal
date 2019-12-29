package io.epifab.tydal.runtime

import java.sql.Connection

import cats.Monad
import cats.effect.{IO, LiftIO, Sync}
import cats.implicits._
import shapeless.HList

import scala.concurrent.{ExecutionContext, Future}

trait Transaction[+Output] {
  def toIO(connection: Connection): IO[Either[DataError, Output]] =
    transact[IO](connection)

  def toFuture(connection: Connection)(implicit executionContext: ExecutionContext): Future[Either[DataError, Output]] =
    toIO(connection).unsafeToFuture()

  def sync(connection: Connection): Either[DataError, Output] =
    toIO(connection).unsafeRunSync()

  final def transact[F[+_]: Sync : Monad : LiftIO](connection: Connection): F[Either[DataError, Output]] =
    ensureNonAutoCommit(connection).flatMap(_ =>
      Sync[F].delay(connection.setSavepoint()).flatMap(savePoint =>
        run(connection).flatMap {
          case Left(error) => Sync[F].delay(connection.rollback(savePoint)).map(_ => Left(error))
          case Right(results) => Sync[F].delay(connection.commit()).map(_ => Right(results))
        }
      )
    )

  private def ensureNonAutoCommit[F[+_]: Sync : Monad](connection: Connection): F[Unit] = for {
    autoCommit <- Sync[F].delay(connection.getAutoCommit)
    _ <- if (autoCommit) Sync[F].delay(connection.setAutoCommit(false)) else Sync[F].pure(())
  } yield ()

  protected def run[F[+_]: Sync : Monad : LiftIO](connection: Connection): F[Either[DataError, Output]]

  final def map[O2](f: Output => O2): Transaction[O2] =
    Transaction.MapTransaction(this, f)

  final def flatMap[O2](f: Output => Transaction[O2]): Transaction[O2] =
    Transaction.FlatMapTransaction(this, f)

  final def discardResults: Transaction[Unit] =
    map(_ => ())
}

object Transaction {
  implicit val monad: Monad[Transaction] = new Monad[Transaction] {
    override def pure[A](x: A): Transaction[A] =
      successful(x)

    override def flatMap[A, B](fa: Transaction[A])(f: A => Transaction[B]): Transaction[B] =
      fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => Transaction[Either[A, B]]): Transaction[B] =
      f(a).flatMap {
        case Left(a) => tailRecM(a)(f)
        case Right(b) => successful(b)
      }
  }

  implicit val liftIO: LiftIO[Transaction] = new LiftIO[Transaction] {
    override def liftIO[A](ioa: IO[A]): Transaction[A] = IOTransaction(ioa.map(Right(_)))
  }

  val unit: Transaction[Unit] = IOTransaction(IO.pure(Right(())))

  def sequence[Output](transactions: Seq[Transaction[Output]]): Transaction[Seq[Output]] =
    new Transaction[Seq[Output]] {
      override protected def run[F[+ _] : Sync : Monad : LiftIO](connection: Connection): F[Either[DataError, Seq[Output]]] = {
        def recursive(ts: Seq[Transaction[Output]]): F[Either[DataError, Seq[Output]]] = ts match {
          case empty if empty.isEmpty => Sync[F].pure(Right(Seq.empty[Output]))
          case nonEmpty => nonEmpty.head.run(connection).flatMap {
            case Left(error) => Sync[F].pure(Left(error))
            case Right(result) => recursive(nonEmpty.tail).map(_.map(seq => result +: seq))
          }
        }
        recursive(transactions)
      }
    }

  def failed(error: DataError): Transaction[Nothing] = IOTransaction(IO.pure(Left(error)))

  def successful[Output](value: Output): Transaction[Output] = IOTransaction(IO.pure(Right(value)))

  case class SimpleTransaction[Fields <: HList, Output](runnableStatement: RunnableStatement[Fields], statementExecutor: StatementExecutor[Connection, Fields, Output]) extends Transaction[Output] {
    override protected def run[F[+ _] : Sync : Monad : LiftIO](connection: Connection): F[Either[DataError, Output]] =
      statementExecutor.run(connection, runnableStatement)
  }

  case class IOTransaction[Output](result: IO[Either[DataError, Output]]) extends Transaction[Output] {
    override protected def run[F[+ _] : Sync : Monad : LiftIO](connection: Connection): F[Either[DataError, Output]] =
      LiftIO[F].liftIO(result)
  }

  case class MapTransaction[O1, Output](transactionIO: Transaction[O1], f: O1 => Output) extends Transaction[Output] {
    override def run[F[+_]: Sync : Monad : LiftIO](connection: Connection): F[Either[DataError, Output]] =
      transactionIO.run(connection).map(_.map(f))
  }

  case class FlatMapTransaction[O1, Output](transaction: Transaction[O1], f: O1 => Transaction[Output]) extends Transaction[Output] {
    override def run[F[+_]: Sync : Monad : LiftIO](connection: Connection): F[Either[DataError, Output]] =
      transaction.run(connection).flatMap {
        case Right(results) => f(results).run(connection)
        case Left(error) => Sync[F].pure(Left(error))
      }
  }

  def apply[Fields <: HList, Output](
    runnableStatement: RunnableStatement[Fields])(
    implicit
    statementExecutor: StatementExecutor[Connection, Fields, Output]
  ): Transaction[Output] = SimpleTransaction(runnableStatement, statementExecutor)
}
