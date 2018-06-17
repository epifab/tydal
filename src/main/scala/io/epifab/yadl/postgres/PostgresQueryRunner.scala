package io.epifab.yadl.postgres

import java.sql.{Connection, PreparedStatement, ResultSet, SQLException}

import cats.Id
import io.epifab.yadl._
import io.epifab.yadl.domain._

import scala.concurrent.{ExecutionContext, Future, blocking}


trait JDBCQueryRunner {
  protected def connection: Connection

  protected def extractResults[T](select: Select, extractor: Extractor[T])(resultSet: ResultSet): Either[ExtractorError, Seq[T]] = {
    import io.epifab.yadl.utils.EitherSupport._

    val rows = scala.collection.mutable.ArrayBuffer.empty[Row]
    while (resultSet.next()) {
      rows += new Row(
        select.fields.zipWithIndex.map {
          case (field, index) =>
            field.alias -> resultSet.getObject(index + 1)
        }.toMap
      )
    }
    firstLeftOrRights(rows.map(extractor))
  }

  protected def preparedStatement(query: Query): PreparedStatement = {
    val statement: PreparedStatement = connection
      .prepareStatement(query.query)

    query.params.zipWithIndex.foreach {
      case (param, index) => statement.setObject(index + 1, param)
    }

    statement
  }
}


class PostgresQueryRunner(protected val connection: Connection, queryBuilder: QueryBuilder[Statement]) extends QueryRunner[Id] with JDBCQueryRunner {
  override def run[T](select: Select)(implicit extractor: Row => Either[ExtractorError, T]): Id[Either[DALError, Seq[T]]] = {
    val statement = preparedStatement(queryBuilder(select))

    try {
      statement.execute()
      extractResults(select, extractor)(statement.executeQuery())
    }
    catch {
      case error: SQLException => Left(DriverError(error))
    }
  }

  override def run(update: Statement with SideEffect): Id[Either[DALError, Int]] = {
    val statement = preparedStatement(queryBuilder(update))

    try {
      statement.execute()
      Right(statement.getUpdateCount)
    }
    catch {
      case error: SQLException => Left(DriverError(error))
    }
  }
}


class AsyncPostgresQueryRunner(protected val connection: Connection, queryBuilder: QueryBuilder[Statement])(implicit executionContext: ExecutionContext) extends QueryRunner[Future] with JDBCQueryRunner {
  override def run[T](select: Select)(implicit extractor: Row => Either[ExtractorError, T]): Future[Either[DALError, Seq[T]]] = {
    val statement = preparedStatement(queryBuilder(select))

    Future {
      try {
        blocking(statement.execute())
        extractResults(select, extractor)(statement.executeQuery())
      }
      catch {
        case error: SQLException => Left(DriverError(error))
      }
    }
  }

  override def run(update: Statement with SideEffect): Future[Either[DALError, Int]] = {
    val statement = preparedStatement(queryBuilder(update))

    Future {
      try {
        blocking(statement.execute())
        Right(statement.getUpdateCount)
      }
      catch {
        case error: SQLException => Left(DriverError(error))
      }
    }
  }
}
