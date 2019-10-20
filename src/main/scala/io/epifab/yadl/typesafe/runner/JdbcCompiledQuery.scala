package io.epifab.yadl.typesafe.runner

import java.sql.{Connection, PreparedStatement, ResultSet}

import io.epifab.yadl.typesafe.Query
import io.epifab.yadl.typesafe.fields.DecoderError

trait CompiledQuery[FIELDS, INPUT, OUTPUT]

class JdbcCompiledQuery[FIELDS, INPUT, OUTPUT]
    (val query: Query, outputDef: FIELDS)
    (implicit
     statement: StatementBuilder[Connection, PreparedStatement, INPUT],
     dataExtractor: DataExtractor[ResultSet, FIELDS, OUTPUT]) extends CompiledQuery[FIELDS, INPUT, OUTPUT] {

  def mapTo[TARGET](implicit dataExtractor: DataExtractor[ResultSet, FIELDS, TARGET]): JdbcCompiledQuery[FIELDS, INPUT, TARGET] =
    new JdbcCompiledQuery(query, outputDef)

  private def extract(resultSet: ResultSet): Either[DecoderError, Seq[OUTPUT]] = {
    import io.epifab.yadl.utils.MonadicOps._

    val iterator = new Iterator[Either[DecoderError, OUTPUT]] {
      override def hasNext: Boolean = resultSet.next()
      override def next(): Either[DecoderError, OUTPUT] = dataExtractor.extract(resultSet, outputDef)
    }

    iterator.toList.getOrFirstError
  }

  def runSync(connection: Connection, input: INPUT): Either[DecoderError, Seq[OUTPUT]] = {
    val preparedStatement = connection.prepareStatement(query.sql)
    statement.build(connection, preparedStatement, query, input)
    extract(preparedStatement.executeQuery())
  }
}
