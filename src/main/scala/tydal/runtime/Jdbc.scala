package tydal.runtime

import cats.effect.Async
import tydal.schema._

import java.sql.{Connection, PreparedStatement}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}

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

  def initStatement[F[_]: Async](
    connection: Connection,
    sql: String,
    placeholderValues: List[Literal[_]]
  ): F[PreparedStatement] = {

    for {
      preparedStatement <- Async[F].blocking(connection.prepareCall(sql))
      _ <- placeholderValues.zipWithIndex.map { case (value, index) =>
        Async[F].blocking(setPlaceholder(
          connection,
          preparedStatement,
          index + 1,
          value.encoder.dbType,
          value.dbValue
        ))
      }.sequence
    } yield preparedStatement
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
