package io.epifab.tydal.runtime

import java.sql.Connection

import cats.effect.{Async, ContextShift, IO, LiftIO, Sync}
import cats.implicits._
import cats.{Applicative, Functor, Monad, Parallel, Traverse}
import org.slf4j.{Logger, LoggerFactory}
import shapeless.HList

import scala.collection.Factory

trait Transaction[+Output] {
  final def transact[F[+_]: Async : Parallel : ContextShift](pool: ConnectionPool[F]): F[Output] = {
    pool.connection.use(connection =>
      for {
        ac <- pool.executor(connection.getAutoCommit)
        _  <- if (ac) pool.executor(connection.setAutoCommit(false)) else Sync[F].pure(())
        sp <- pool.executor(connection.setSavepoint())
        rs <- Async[F].attemptTap(run(connection, pool.executor)) {
          case Left(_) => pool.executor(connection.rollback(sp))
          case Right(_) => pool.executor(connection.commit())
        }
      } yield rs)
  }

  protected def run[F[+_] : Async : Parallel : ContextShift](connection: Connection, jdbcExecutor: JdbcExecutor): F[Output]

  final def map[O2](f: Output => O2): Transaction[O2] =
    Transaction.MapTransaction(this, f)

  final def flatMap[O2](f: Output => Transaction[O2]): Transaction[O2] =
    Transaction.FlatMapTransaction(this, f)

  final def discardResults: Transaction[Unit] = map(_ => ())
}

object Transaction {

  implicit val monad: Monad[Transaction] = new Monad[Transaction] {
    override def pure[A](x: A): Transaction[A] =
      Transaction.successful(x)

    override def flatMap[A, B](fa: Transaction[A])(f: A => Transaction[B]): Transaction[B] =
      fa.flatMap(f)

    override def map[A, B](fa: Transaction[A])(f: A => B): Transaction[B] =
      applicative.map(fa)(f)

    override def tailRecM[A, B](a: A)(f: A => Transaction[Either[A, B]]): Transaction[B] =
      f(a).flatMap {
        case Left(a) => tailRecM(a)(f)
        case Right(b) => successful(b)
      }
  }

  implicit val applicative: Applicative[Transaction] = new Applicative[Transaction] {
    override def pure[A](x: A): Transaction[A] =
      Transaction.successful(x)

    override def ap[A, B](ff: Transaction[A => B])(fa: Transaction[A]): Transaction[B] =
      new Transaction[B] {
        override protected def run[F[+_] : Async : Parallel : ContextShift](connection: Connection, jdbcExecutor: JdbcExecutor): F[B] =
          (ff.run(connection, jdbcExecutor), fa.run(connection, jdbcExecutor)).parMapN { case (f, a) => f(a) }
      }
  }

  implicit val liftIO: LiftIO[Transaction] = new LiftIO[Transaction] {
    override def liftIO[A](ioa: IO[A]): Transaction[A] = IOTransaction(ioa)
  }

  def fromIO[A](ioa: IO[A]): Transaction[A] = liftIO.liftIO(ioa)

  def sequential[C[_] : Traverse, Output](transactions: C[Transaction[Output]]): Transaction[C[Output]] =
    Traverse[C].sequence(transactions)(monad)

  def vector[Output](transactions: Vector[Transaction[Output]]): Transaction[Vector[Output]] =
    sequential(transactions)

  def list[Output](transactions: List[Transaction[Output]]): Transaction[List[Output]] =
    sequential(transactions)

  def parallel[C[_] : Traverse, Output](transactions: C[Transaction[Output]])(implicit factory: Factory[Output, C[Output]]): Transaction[C[Output]] =
    ParTransactions(transactions)

  def parVector[Output](transactions: Vector[Transaction[Output]]): Transaction[Vector[Output]] =
    parallel(transactions)

  def parList[Output](transactions: List[Transaction[Output]]): Transaction[List[Output]] =
    parallel(transactions)

  val unit: Transaction[Unit] = IOTransaction(IO.pure(Right(())))

  def failed(error: RuntimeException): Transaction[Nothing] = IOTransaction(IO.raiseError(error))

  def successful[Output](value: Output): Transaction[Output] = IOTransaction(IO.pure(value))

  case class SimpleTransaction[Fields <: HList, Output](runnableStatement: RunnableStatement[Fields], statementExecutor: StatementExecutor[Connection, Fields, Output]) extends Transaction[Output] {
    val log: Logger = LoggerFactory.getLogger(getClass.getName)

    override protected def run[F[+_]: Async : Parallel : ContextShift](connection: Connection, jdbcExecutor: JdbcExecutor): F[Output] = {
      for {
        _ <- Sync[F].delay(log.debug(s"Running query: ${runnableStatement.sql}"))
        res <- Sync[F].attemptTap(statementExecutor.run(connection, jdbcExecutor, runnableStatement)) {
          case Left(error) => Sync[F].delay(log.error(s"Query ${runnableStatement.sql}: failed with ${error.getMessage}"))
          case Right(_) => Sync[F].delay(log.debug(s"Query ${runnableStatement.sql}: succeeded"))
        }
      } yield res
    }
  }

  case class IOTransaction[Output](result: IO[Output]) extends Transaction[Output] {
    override protected def run[F[+_]: Async : Parallel : ContextShift](connection: Connection, jdbcExecutor: JdbcExecutor): F[Output] =
      LiftIO[F].liftIO(result)
  }

  case class MapTransaction[O1, Output](transactionIO: Transaction[O1], f: O1 => Output) extends Transaction[Output] {
    override def run[F[+_]: Async : Parallel : ContextShift](connection: Connection, jdbcExecutor: JdbcExecutor): F[Output] =
      transactionIO.run(connection, jdbcExecutor).map(f)
  }

  case class FlatMapTransaction[O1, Output](transaction: Transaction[O1], f: O1 => Transaction[Output]) extends Transaction[Output] {
    override def run[F[+_]: Async : Parallel : ContextShift](connection: Connection, jdbcExecutor: JdbcExecutor): F[Output] =
      transaction.run(connection, jdbcExecutor).flatMap(f(_).run(connection, jdbcExecutor))
  }

  case class ParTransactions[C[_]: Traverse : Functor, Output](transactions: C[Transaction[Output]])(implicit factory: Factory[Output, C[Output]]) extends Transaction[C[Output]] {
    override def run[F[+_]: Async : Parallel : ContextShift](connection: Connection, jdbcExecutor: JdbcExecutor): F[C[Output]] = {
      transactions
        .map(_.run[F](connection, jdbcExecutor))
        .parSequence
    }
  }

  def apply[Fields <: HList, Output](
    runnableStatement: RunnableStatement[Fields])(
    implicit
    statementExecutor: StatementExecutor[Connection, Fields, Output]
  ): Transaction[Output] = SimpleTransaction(runnableStatement, statementExecutor)
}
