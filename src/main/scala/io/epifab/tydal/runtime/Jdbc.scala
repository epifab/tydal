package io.epifab.tydal.runtime

import java.sql.{Connection, PreparedStatement}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}

import cats.effect.{ContextShift, Sync}
import cats.{Monad, Traverse}
import io.epifab.tydal.schema._
import io.epifab.tydal.utils.EitherSupport

object SqlDateTime {
  val formatter: DateTimeFormatter = new DateTimeFormatterBuilder()
    .appendPattern("yyyy-MM-dd")
    .appendLiteral("T")
    .appendPattern("HH:mm:ss.SSSSSSSSS")
    .toFormatter()

  val parser: DateTimeFormatter = new DateTimeFormatterBuilder()
    .appendPattern("yyyy-MM-dd")
    .appendLiteral(" ")
    .appendPattern("HH:mm:ss")
    // optional nanos, with 9, 6 or 3 digits
    .appendPattern("[.SSSSSSSSS][.SSSSSSSS][.SSSSSSS][.SSSSSS][.SSSSS][.SSSS][.SSS][.SS][.S]")
    .toFormatter()
}

object SqlDate {
  val formatter: DateTimeFormatter = new DateTimeFormatterBuilder()
    .appendPattern("yyyy-MM-dd")
    .toFormatter()

  val parser: DateTimeFormatter = formatter
}

object Jdbc {
  import cats.implicits._

  def initStatement[F[_] : Sync : ContextShift](
    connection: Connection,
    jdbcExecutor: JdbcExecutor,
    sql: String,
    placeholderValues: List[Literal[_]]
  ): F[Either[DataError, PreparedStatement]] = {

    jdbcExecutor.safe(connection.prepareStatement(sql)) flatMap {
      case Right(preparedStatement) =>

        val results: F[List[Either[DataError, Unit]]] = Traverse[List].sequence(placeholderValues.zipWithIndex.map {
          case (value, index) => jdbcExecutor.safe(setPlaceholder(
            connection,
            preparedStatement,
            index + 1,
            value.encoder.dbType,
            value.dbValue
          ))
        })

        results.map(x => EitherSupport.leftOrRights[List, DataError, Unit](x))
          .map(_.map(_ => preparedStatement))

      case Left(error) => Monad[F].pure(Left(error))
    }
  }

  @scala.annotation.tailrec
  private def setPlaceholder[U, X](connection: Connection, statement: PreparedStatement, index: Int, dbType: FieldType[U], value: X): Unit = {
    dbType match {
      case TypeInt =>
        statement.setInt(index, value.asInstanceOf[Int])

      case TypeLong =>
        statement.setLong(index, value.asInstanceOf[Long])

      case TypeDouble =>
        statement.setDouble(index, value.asInstanceOf[Double])

      case TypeSeq(innerType) =>
        setPlaceholderSeq(connection, statement, index, innerType, value.asInstanceOf[Seq[_]])

      case TypeOption(innerDbType) =>
        value match {
          case Some(v) => setPlaceholder(connection, statement, index, innerDbType, v)
          case None => statement.setObject(index, null)
        }

      case _ => statement.setObject(index, value)
    }
  }

  private def setPlaceholderSeq[U](connection: Connection, statement: PreparedStatement, index: Int, dbType: FieldType[U], value: Seq[U]): Unit = {
    val array: java.sql.Array = connection.createArrayOf(
      dbType.sqlName,
      value.toArray
    )
    statement.setArray(index, array)
  }
}
