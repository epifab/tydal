package io.epifab.dal

import java.sql.{Connection, PreparedStatement, ResultSet, SQLException}

import domain.{DriverError, SelectQuery}

import scala.concurrent.{ExecutionContext, Future}

class PostgresQueryRunner(connection: Connection, queryBuilder: QueryBuilder[SelectQuery])(implicit executionContext: ExecutionContext) extends QueryRunner[Future] {
  override def select(select: SelectQuery): Future[Either[DriverError, Seq[Row]]] = {
    val queryAndParameters = queryBuilder(select)

    val statement: PreparedStatement = connection
      .prepareStatement(queryAndParameters.query)

    queryAndParameters.params.zipWithIndex.foreach { case (param, index) => statement.setObject(index + 1, param) }

    def resultSetToRow(resultSet: ResultSet): Seq[Row] = {
      val rows = scala.collection.mutable.ArrayBuffer.empty[Row]
      while (resultSet.next()) {
        rows += new Row(
          select.fields.zipWithIndex.map {
            case (field, index) =>
              field.alias -> resultSet.getObject(index + 1)
          }.toMap
        )
      }
      rows
    }

    Future {
      try {
        statement.execute()
        Right(resultSetToRow(statement.executeQuery()))
      }
      catch {
        case error: SQLException => Left(DriverError(error))
      }
    }
  }
}
