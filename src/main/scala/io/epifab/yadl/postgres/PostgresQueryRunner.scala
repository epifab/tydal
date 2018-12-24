package io.epifab.yadl.postgres

import java.sql.{Connection, PreparedStatement, ResultSet, SQLException}

import cats.Id
import io.epifab.yadl.domain._
import io.epifab.yadl.utils.LoggingSupport

import scala.concurrent.{ExecutionContext, Future, blocking}


trait JDBCQueryRunner {
  protected def connection: Connection

  private def setParameter[T](statement: PreparedStatement, index: Integer, value: Value[T]): Unit = {
    def set[U](index: Int, dbValue: U, dbType: DbType[U]): Any = dbType match {
      case StringDbType | DateDbType | DateTimeDbType | JsonDbType | EnumDbType(_) =>
        statement.setObject(index, dbValue)

      case IntDbType =>
        statement.setInt(index, dbValue)

      case DoubleDbType =>
        statement.setDouble(index, dbValue)

      case StringSeqDbType =>
        val array: java.sql.Array = connection.createArrayOf(
          "varchar",
          dbValue.toArray
        )
        statement.setArray(index, array)

      case IntSeqDbType =>
        val array: java.sql.Array = connection.createArrayOf(
          "integer",
          dbValue.map((i: Int) => new Integer(i)).toArray
        )
        statement.setArray(index, array)

      case DoubleSeqDbType =>
        val array: java.sql.Array = connection.createArrayOf(
          "double",
          dbValue.map(new java.lang.Double(_)).toArray
        )
        statement.setArray(index, array)

      case OptionDbType(innerType) =>
        dbValue match {
          case None =>
            statement.setObject(index, null)

          case Some(innerValue) =>
            set(index, innerValue, innerType)
        }
    }

    set(index, value.dbValue, value.adapter.dbType)
  }

  private def getColumn[T](resultSet: ResultSet, index: Int)(implicit adapter: FieldAdapter[T]): Either[ExtractorError, T] = {
    def get[U](index: Int, dbType: DbType[U]): U = dbType match {
      case StringDbType | DateDbType | DateTimeDbType | JsonDbType | EnumDbType(_) =>
        resultSet.getObject(index).toString

      case IntDbType =>
        resultSet.getInt(index)

      case DoubleDbType =>
        resultSet.getDouble(index)

      case StringSeqDbType =>
        resultSet.getArray(index)
          .getArray
          .asInstanceOf[Array[String]]
          .toSeq

      case IntSeqDbType =>
        resultSet.getArray(index)
          .getArray
          .asInstanceOf[Array[Integer]]
          .toSeq
          .map(_.toInt)

      case DoubleSeqDbType =>
        resultSet.getArray(index)
          .getArray
          .asInstanceOf[Array[Double]]
          .toSeq
          .map(_.toDouble)

      case OptionDbType(innerType) =>
        Option(resultSet.getObject(index)).map(_ => get(index, innerType))
    }

    adapter.fromDb(get(index, adapter.dbType))
  }

  protected def extractResults[T](select: Select, extractor: Extractor[T])(resultSet: ResultSet): Either[ExtractorError, Seq[T]] = {
    import io.epifab.yadl.utils.EitherSupport._

    val columnIndexes: Map[Column[_], Int] =
      (select.columns ++ select.aggregations).zipWithIndex.toMap

    val results = scala.collection.mutable.ArrayBuffer.empty[Either[ExtractorError, T]]

    while (resultSet.next()) {
      val row = new Row {
        override def get[FT](column: Column[FT]): Either[ExtractorError, FT] =
          columnIndexes.get(column) match {
            case Some(index) =>
              getColumn(resultSet, index + 1)(column.adapter)
            case None =>
              Left(ExtractorError(s"Column $column is missing"))
          }
      }
      results += extractor(row)
    }

    firstLeftOrRights(results)
  }

  protected def preparedStatement(query: Query): PreparedStatement = {
    val statement: PreparedStatement = connection
      .prepareStatement(query.sql)

    query.params.zipWithIndex.foreach {
      case (value, index) =>
        setParameter(statement, index + 1, value)
    }

    statement
  }
}


class PostgresQueryRunner(protected val connection: Connection, queryBuilder: QueryBuilder[Statement]) extends QueryRunner[Id] with JDBCQueryRunner with LoggingSupport {
  override def run[T](select: Select)(implicit extractor: Row => Either[ExtractorError, T]): Id[Either[DALError, Seq[T]]] = {
    val query = queryBuilder(select)
    val statement = preparedStatement(query)

    try {
      extractResults(select, extractor)(statement.executeQuery())
    }
    catch {
      case error: SQLException =>
        withMdc(Map("query" -> query.sql)) { log.error("Could not run SQL query", error) }
        Left(DriverError(error))
    }
  }

  override def run(update: Statement with SideEffect): Id[Either[DALError, Int]] = {
    val query = queryBuilder(update)
    val statement = preparedStatement(query)

    try {
      statement.execute()
      Right(statement.getUpdateCount)
    }
    catch {
      case error: SQLException =>
        withMdc(Map("query" -> query.sql)) { log.error("Could not run SQL query", error) }
        Left(DriverError(error))
    }
  }
}


class AsyncPostgresQueryRunner(protected val connection: Connection, queryBuilder: QueryBuilder[Statement])(implicit executionContext: ExecutionContext) extends QueryRunner[Future] with JDBCQueryRunner with LoggingSupport {
  override def run[T](select: Select)(implicit extractor: Row => Either[ExtractorError, T]): Future[Either[DALError, Seq[T]]] = {
    val query = queryBuilder(select)
    val statement = preparedStatement(query)

    Future {
      try {
        extractResults(select, extractor)(blocking(statement.executeQuery))
      }
      catch {
        case error: SQLException =>
          withMdc(Map("query" -> query.sql)) { log.error("Could not run SQL query", error) }
          Left(DriverError(error))
      }
    }
  }

  override def run(update: Statement with SideEffect): Future[Either[DALError, Int]] = {
    val query = queryBuilder(update)
    val statement = preparedStatement(query)

    Future {
      try {
        blocking(statement.execute())
        Right(statement.getUpdateCount)
      }
      catch {
        case error: SQLException =>
          withMdc(Map("query" -> query.sql)) { log.error("Could not run SQL query", error) }
          Left(DriverError(error))
      }
    }
  }
}
