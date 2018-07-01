package io.epifab.yadl.postgres

import java.sql.{Connection, PreparedStatement, ResultSet, SQLException}

import cats.Id
import io.epifab.yadl.domain._

import scala.concurrent.{ExecutionContext, Future, blocking}


trait JDBCQueryRunner {
  protected def connection: Connection

  private def setParameter[T](statement: PreparedStatement, index: Integer, value: Value[T]): Unit =
    (value.adapter.dbType: DbType[_]) match {
      case IntDbType =>
        statement.setInt(index, value.dbValue.asInstanceOf[Int])

      case StringDbType =>
        statement.setString(index, value.dbValue.asInstanceOf[String])

      case SeqDbType(dbType) =>
        (dbType: PrimitiveDbType[_]) match {
          case IntDbType =>
            val array: java.sql.Array = connection.createArrayOf(
              "int",
              value.dbValue.asInstanceOf[Seq[Int]].toArray.map(new Integer(_))
            )
            statement.setArray(index, array)

          case StringDbType =>
            val array: java.sql.Array = connection.createArrayOf(
              "varchar",
              value.dbValue.asInstanceOf[Seq[String]].toArray
            )
            statement.setArray(index, array)
        }

      case OptionDbType(dbType) =>
        value.dbValue.asInstanceOf[Option[dbType.DBTYPE]] match {
          case None => statement.setObject(index, null)
          case Some(dbValue) =>
            (dbType: PrimitiveDbType[_]) match {
              case IntDbType =>
                statement.setInt(index, dbValue.asInstanceOf[Int])
              case StringDbType =>
                statement.setString(index, dbValue.asInstanceOf[String])
            }
        }
    }

  private def getColumn[T](resultSet: ResultSet, index: Int)(implicit fieldAdapter: FieldAdapter[T]): Either[ExtractorError, T] =
    ???

  protected def extractResults[T](select: Select, extractor: Extractor[T])(resultSet: ResultSet): Either[ExtractorError, Seq[T]] = {
    import io.epifab.yadl.utils.EitherSupport._

    val fieldIndexes: Map[Field[_], Int] =
      select.fields.zipWithIndex.toMap

    val results = scala.collection.mutable.ArrayBuffer.empty[Either[ExtractorError, T]]

    while (resultSet.next()) {
      val row = new Row {
        override def get[FT](field: Field[FT]): Either[ExtractorError, FT] =
          fieldIndexes.get(field) match {
            case Some(index) =>
              getColumn(resultSet, index + 1)(field.adapter)
            case None =>
              Left(ExtractorError(s"Field `${field.src}` is missing"))
          }
      }
      results += extractor(row)
    }

    firstLeftOrRights(results)
  }

  protected def preparedStatement(query: Query): PreparedStatement = {
    val statement: PreparedStatement = connection
      .prepareStatement(query.query)

    query.params.zipWithIndex.foreach {
      case (value, index) =>
        setParameter(statement, index + 1, value)
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
