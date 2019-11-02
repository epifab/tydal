package io.epifab.yadl.runner

import java.sql.Connection

import cats.Functor
import cats.effect.IO
import cats.implicits._
import io.epifab.yadl._
import io.epifab.yadl.runner.TransactionIO.{FlatMapTransactionIO, MapTransactionIO}
import shapeless.ops.hlist.Tupler
import shapeless.{Generic, HList}

trait TransactionIO[OUTPUT] {
  final def transact(connection: Connection): IOEither[DataError, OUTPUT] =
    ensureNonAutoCommit(connection).flatMap(_ =>
      IO(connection.setSavepoint()).flatMap(savePoint =>
        run(connection).flatMap {
          case Left(error) => IO(connection.rollback(savePoint)).map(_ => Left(error))
          case Right(results) => IO(connection.commit()).map(_ => Right(results))
        }
      )
    )

  private def ensureNonAutoCommit(connection: Connection): IO[Unit] = for {
    autoCommit <- IO(connection.getAutoCommit)
    _ <- if (autoCommit) IO(connection.setAutoCommit(false)) else IO.pure(())
  } yield ()

  protected def run(connection: Connection): IOEither[DataError, OUTPUT]

  final def map[O2](f: OUTPUT => O2): TransactionIO[O2] =
    new MapTransactionIO(this, f)

  final def flatMap[O2](f: OUTPUT => TransactionIO[O2]): TransactionIO[O2] =
    new FlatMapTransactionIO(this, f)
}

object TransactionIO {
  // todo: Seq is not a good abstraction. Vector/List would be better
  implicit val seqFunctor: Functor[Seq] = new Functor[Seq] {
    override def map[A, B](fa: Seq[A])(f: A => B): Seq[B] = fa.map(f)
  }

  class ExtendedTransactionIO[F[_]: Functor, OUTPUT <: HList](transactionIO: TransactionIO[F[OUTPUT]]) {
    def mapTo[O2](implicit generic: Generic.Aux[O2, OUTPUT]): TransactionIO[F[O2]] =
      transactionIO.map(_.map(generic.from))

    def tuple[TP](implicit t: Tupler.Aux[OUTPUT, TP], generic: Generic.Aux[TP, OUTPUT]): TransactionIO[F[TP]] =
      mapTo[TP]
  }

  implicit class ExtendedOptTransactionIO[OUTPUT <: HList](transactionIO: TransactionIO[Option[OUTPUT]])
    extends ExtendedTransactionIO[Option, OUTPUT](transactionIO)

  implicit class ExtendedSeqTransactionIO[OUTPUT <: HList](transactionIO: TransactionIO[Seq[OUTPUT]])
    extends ExtendedTransactionIO[Seq, OUTPUT](transactionIO) {
    def takeFirst: TransactionIO[Option[OUTPUT]] = transactionIO.map(_.headOption)
  }

  class MapTransactionIO[O1, OUTPUT](transactionIO: TransactionIO[O1], f: O1 => OUTPUT) extends TransactionIO[OUTPUT] {
    override def run(connection: Connection): IOEither[DataError, OUTPUT] =
      transactionIO.run(connection).map(_.map(f))
  }

  class FlatMapTransactionIO[O1, OUTPUT](transactionIO: TransactionIO[O1], f: O1 => TransactionIO[OUTPUT]) extends TransactionIO[OUTPUT] {
    override def run(connection: Connection): IOEither[DataError, OUTPUT] =
      transactionIO.run(connection).flatMap {
        case Right(results) => f(results).run(connection)
        case Left(error) => IO.pure(Left(error))
      }
  }

  def apply[FIELDS <: HList, OUTPUT]
  (runnableStatement: RunnableStatement[FIELDS])
  (implicit statementExecutor: StatementExecutor[IOEither, Connection, FIELDS, OUTPUT]): TransactionIO[OUTPUT] =
    (connection: Connection) => statementExecutor.run(connection, runnableStatement)
}
