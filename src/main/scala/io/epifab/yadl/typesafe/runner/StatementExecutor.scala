package io.epifab.yadl.typesafe.runner

import java.sql.{Connection, PreparedStatement, ResultSet}

import io.epifab.yadl.typesafe.fields._
import io.epifab.yadl.typesafe.{DataError, DecoderError, DriverError}
import shapeless.HList

import scala.util.Try
import scala.util.control.NonFatal

trait StatementExecutor[F[+_, +_], CONN, FIELDS <: HList, OUTPUT] {
  def run(connection: CONN, statement: Statement[FIELDS]): F[DataError, Seq[OUTPUT]]
}

object StatementExecutor {
  implicit def syncJDBC[FIELDS <: HList, OUTPUT]
    (implicit dataExtractor: DataExtractor[ResultSet, FIELDS, OUTPUT]): StatementExecutor[Either, Connection, FIELDS, OUTPUT] =

  new StatementExecutor[Either, Connection, FIELDS, OUTPUT] {
    override def run(connection: Connection, statement: Statement[FIELDS]): Either[DataError, Seq[OUTPUT]] = {
      for {
        preparedStatement <- initStatement(connection, statement.sql, statement.input)
        results <- runStatement(preparedStatement, statement.fields)
      } yield results
    }

    private def initStatement(connection: Connection, sql: String, placeholderValues: Seq[Value[_]]): Either[DriverError, PreparedStatement] =
      Try(connection.prepareStatement(sql)).toEither match {
        case Right(preparedStatement) =>
          placeholderValues.zipWithIndex.foreach {
            case (value, index) => setPlaceholder(
              connection,
              preparedStatement,
              index + 1,
              value.encoder.dbType,
              value.dbValue
            )
          }
          Right(preparedStatement)

        case Left(NonFatal(e)) =>
          Left(DriverError(e.getMessage))

        case Left(fatalError) =>
          throw fatalError
      }

    private def runStatement(preparedStatement: PreparedStatement, fields: FIELDS): Either[DataError, Seq[OUTPUT]] =
      Try(preparedStatement.executeQuery()).toEither match {
        case Right(resultSet) => extract(fields, resultSet)
        case Left(NonFatal(e)) => Left(DriverError(e.getMessage))
        case Left(fatalError) => throw fatalError
      }

    private def setPlaceholderSeq[U](connection: Connection, statement: PreparedStatement, index: Int, dbType: FieldType[U], value: Seq[U]): Unit = {
      val array: java.sql.Array = connection.createArrayOf(
        dbType.sqlName,
        value.toArray
      )
      statement.setArray(index, array)
    }

    @scala.annotation.tailrec
    private def setPlaceholder[U, X](connection: Connection, statement: PreparedStatement, index: Int, dbType: FieldType[U], value: X): Unit = {
      dbType match {
        case TypeString | TypeDate | TypeDateTime | TypeJson | TypeEnum(_) | TypeGeography | TypeGeometry =>
          statement.setObject(index, value)

        case TypeInt =>
          statement.setInt(index, value.asInstanceOf[Int])

        case TypeDouble =>
          statement.setDouble(index, value.asInstanceOf[Double])

        case TypeSeq(innerType) =>
          setPlaceholderSeq(connection, statement, index, innerType, value.asInstanceOf[Seq[_]])

        case TypeOption(innerDbType) =>
          value match {
            case Some(v) => setPlaceholder(connection, statement, index, innerDbType, v)
            case None => statement.setObject(index, null)
          }
      }
    }

    private def extract(fields: FIELDS, resultSet: ResultSet): Either[DecoderError, Seq[OUTPUT]] = {
      import io.epifab.yadl.utils.MonadicOps._

      val iterator = new Iterator[Either[DecoderError, OUTPUT]] {
        override def hasNext: Boolean = resultSet.next()
        override def next(): Either[DecoderError, OUTPUT] = dataExtractor.extract(resultSet, fields)
      }

      iterator.toList.getOrFirstError
    }
  }
}
