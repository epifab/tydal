package io.epifab.yadl.typesafe

import java.sql.Connection

import cats.effect.IO
import io.epifab.yadl.typesafe.TransactionIO.{FlatMapTransactionIO, MapTransactionIO}
import io.epifab.yadl.typesafe.runner.{RunnableStatement, StatementExecutor}
import shapeless.{Generic, HList}
import shapeless.ops.hlist.Tupler

trait TransactionIO[OUTPUT] {
  def transact(connection: Connection): IOEither[DataError, OUTPUT]

  final def map[O2](f: OUTPUT => O2): TransactionIO[O2] =
    new MapTransactionIO(this, f)

  final def flatMap[O2](f: OUTPUT => TransactionIO[O2]): TransactionIO[O2] =
    new FlatMapTransactionIO(this, f)
}

object TransactionIO {
  implicit class ExtendedSeqTransactionIO[OUTPUT <: HList](transactionIO: TransactionIO[Seq[OUTPUT]]) {
    def option: TransactionIO[Option[OUTPUT]] =
      transactionIO.map(_.headOption)

    def mapTo[O2](implicit generic: Generic.Aux[O2, OUTPUT]): TransactionIO[Seq[O2]] =
      transactionIO.map(_.map(generic.from))

    def toTuple[TP](implicit t: Tupler.Aux[OUTPUT, TP], generic: Generic.Aux[TP, OUTPUT]): TransactionIO[Seq[TP]] =
      mapTo[TP]
  }

  implicit class ExtendedTransactionIO[OUTPUT <: HList](transactionIO: TransactionIO[Option[OUTPUT]]) {
    def mapTo[O2](implicit generic: Generic.Aux[O2, OUTPUT]): TransactionIO[Option[O2]] =
      transactionIO.map(_.map(generic.from))

    def toTuple[TP](implicit t: Tupler.Aux[OUTPUT, TP], generic: Generic.Aux[TP, OUTPUT]): TransactionIO[Option[TP]] =
      mapTo[TP]
  }

  class MapTransactionIO[O1, OUTPUT](transactionIO: TransactionIO[O1], f: O1 => OUTPUT) extends TransactionIO[OUTPUT] {
    override def transact(connection: Connection): IOEither[DataError, OUTPUT] =
      transactionIO.transact(connection).map(_.map(f))
  }

  class FlatMapTransactionIO[O1, OUTPUT](transactionIO: TransactionIO[O1], f: O1 => TransactionIO[OUTPUT]) extends TransactionIO[OUTPUT] {
    override def transact(connection: Connection): IOEither[DataError, OUTPUT] =
      transactionIO.transact(connection).flatMap {
        case Right(results) => f(results).transact(connection)
        case Left(error) => IO.pure(Left(error))
      }
  }

  def apply[FIELDS <: HList, OUTPUT]
  (runnableStatement: RunnableStatement[FIELDS])
  (implicit statementExecutor: StatementExecutor[IOEither, Connection, FIELDS, OUTPUT]): TransactionIO[OUTPUT] =
    (connection: Connection) => statementExecutor.run(connection, runnableStatement)
}
