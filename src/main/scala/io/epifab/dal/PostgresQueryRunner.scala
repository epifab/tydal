package io.epifab.dal

import java.sql.{Connection, PreparedStatement, ResultSet, SQLException}

import io.epifab.dal.domain.{DALError, DriverError, ExtractorError, Select}

import scala.concurrent.{ExecutionContext, Future}


class PostgresQueryRunner(connection: Connection, queryBuilder: QueryBuilder[Select])(implicit executionContext: ExecutionContext) extends QueryRunner[Future] {
  private def extractResults[T](select: Select, extractor: Row => Either[ExtractorError, T])(resultSet: ResultSet): Either[ExtractorError, Seq[T]] = {
    import io.epifab.dal.utils.EitherSupport._

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

  private def preparedStatement(select: Select): PreparedStatement = {
    val queryAndParameters = queryBuilder(select)

    val statement: PreparedStatement = connection
      .prepareStatement(queryAndParameters.query)

    queryAndParameters.params.zipWithIndex.foreach {
      case (param, index) => statement.setObject(index + 1, param)
    }

    statement
  }

  override def selectAll[T](select: Select)(implicit extractor: Row => Either[ExtractorError, T]): Future[Either[DALError, Seq[T]]] = {
    val statement = preparedStatement(select)

    Future {
      try {
        statement.execute()
        extractResults(select, extractor)(statement.executeQuery())
      }
      catch {
        case error: SQLException => Left(DriverError(error))
      }
    }
  }
}
