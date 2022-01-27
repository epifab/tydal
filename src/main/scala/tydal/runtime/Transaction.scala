package tydal.runtime

import cats.arrow.FunctionK
import cats.effect.{Async, IO, LiftIO}
import cats.implicits._
import cats.{Applicative, Functor, Monad, MonadError, Parallel, Traverse}
import org.slf4j.{Logger, LoggerFactory}
import shapeless.HList

import java.sql.Connection
import scala.collection.Factory

trait Transaction[+Output] {
  final def transact[F[+_]: Async : Parallel: LiftIO](pool: ConnectionPool[F]): F[Output] = {
    pool.connection.use(connection =>
      for {
        ac <- Async[F].blocking(connection.getAutoCommit)
        _  <- if (ac) Async[F].blocking(connection.setAutoCommit(false)) else Async[F].pure(())
        sp <- Async[F].blocking(connection.setSavepoint())
        rs <- Async[F].attemptTap(run(connection)) {
          case Left(_) => Async[F].blocking(connection.rollback(sp))
          case Right(_) => Async[F].blocking(connection.commit())
        }
      } yield rs)
  }

  protected def run[F[+_]: Async : Parallel: LiftIO](connection: Connection): F[Output]

  final def map[O2](f: Output => O2): Transaction[O2] =
    Transaction.MapTransaction(this, f)

  final def flatMap[O2](f: Output => Transaction[O2]): Transaction[O2] =
    Transaction.FlatMapTransaction(this, f)

  final def attempt: Transaction[Either[Throwable, Output]] =
    Transaction.AttemptiveTransaction(this)

  final def attemptTap[F[_]: Async](f: PartialFunction[Either[Throwable, Output], F[Unit]])(implicit lift: FunctionK[F, Transaction]): Transaction[Output] =
    attempt
      .flatMap {
        case x if f.isDefinedAt(x) => lift(f(x)).map(_ => x)
        case x => Transaction.successful(x)
      }
      .flatMap {
        case Left(x) => Transaction.failed(x)
        case Right(y) => Transaction.successful(y)
      }

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
        override protected def run[F[+_]: Async : Parallel: LiftIO](connection: Connection): F[B] =
          (ff.run(connection), fa.run(connection)).parMapN { case (f, a) => f(a) }
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

  def failed(error: Throwable): Transaction[Nothing] = IOTransaction(IO.raiseError(error))

  def successful[Output](value: Output): Transaction[Output] = IOTransaction(IO.pure(value))

  case class SimpleTransaction[Fields <: HList, Output](runnableStatement: RunnableStatement[Fields], statementExecutor: StatementExecutor[Connection, Fields, Output]) extends Transaction[Output] {
    val log: Logger = LoggerFactory.getLogger(getClass.getName)

    override protected def run[F[+_]: Async: Parallel: LiftIO](connection: Connection): F[Output] = {
      for {
        _ <- Async[F].delay(log.debug(s"Running query: ${runnableStatement.sql}"))
        res <- Async[F].attemptTap(statementExecutor.run(connection, runnableStatement)) {
          case Left(error) => Async[F].delay(log.error(s"Query ${runnableStatement.sql}: failed with ${error.getMessage}"))
          case Right(_) => Async[F].delay(log.debug(s"Query ${runnableStatement.sql}: succeeded"))
        }
      } yield res
    }
  }

  case class IOTransaction[Output](result: IO[Output]) extends Transaction[Output] {
    override protected def run[F[+_]: Async: Parallel: LiftIO](connection: Connection): F[Output] =
      LiftIO[F].liftIO(result)
  }

  case class MapTransaction[O1, Output](transaction: Transaction[O1], f: O1 => Output) extends Transaction[Output] {
    override def run[F[+_]: Async: Parallel: LiftIO](connection: Connection): F[Output] =
      transaction.run(connection).map(f)
  }

  case class FlatMapTransaction[O1, Output](transaction: Transaction[O1], f: O1 => Transaction[Output]) extends Transaction[Output] {
    override def run[F[+_]: Async: Parallel: LiftIO](connection: Connection): F[Output] =
      transaction.run(connection).flatMap(f(_).run(connection))
  }

  case class ParTransactions[C[_]: Traverse : Functor, Output](transactions: C[Transaction[Output]])(implicit factory: Factory[Output, C[Output]]) extends Transaction[C[Output]] {
    override def run[F[+_]: Async: Parallel: LiftIO](connection: Connection): F[C[Output]] = {
      transactions
        .map(_.run[F](connection))
        .parSequence
    }
  }

  case class AttemptiveTransaction[Output](transaction: Transaction[Output]) extends Transaction[Either[Throwable, Output]] {
    override protected def run[F[+_]: Async: Parallel: LiftIO](connection: Connection): F[Either[Throwable, Output]] =
      Async[F].attempt(transaction.run(connection))
  }

  implicit val monadError: MonadError[Transaction, Throwable] = new MonadError[Transaction, Throwable] {
    override def flatMap[A, B](fa: Transaction[A])(f: A => Transaction[B]): Transaction[B] =
      monad.flatMap(fa)(f)

    override def tailRecM[A, B](a: A)(f: A => Transaction[Either[A, B]]): Transaction[B] =
      monad.tailRecM(a)(f)

    override def raiseError[A](e: Throwable): Transaction[A] =
      Transaction.failed(e)

    override def handleErrorWith[A](fa: Transaction[A])(f: Throwable => Transaction[A]): Transaction[A] =
      fa.attempt.flatMap {
        case Left(error) => f(error)
        case Right(ok) => Transaction.successful(ok)
      }

    override def pure[A](x: A): Transaction[A] =
      monad.pure(x)
  }

  implicit val transactionIO: FunctionK[IO, Transaction] = new FunctionK[IO, Transaction] {
    override def apply[A](fa: IO[A]): Transaction[A] = Transaction.fromIO(fa)
  }

  def apply[Fields <: HList, Output](
    runnableStatement: RunnableStatement[Fields])(
    implicit
    statementExecutor: StatementExecutor[Connection, Fields, Output]
  ): Transaction[Output] = SimpleTransaction(runnableStatement, statementExecutor)
}
