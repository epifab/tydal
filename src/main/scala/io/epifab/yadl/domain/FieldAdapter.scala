package io.epifab.yadl.domain

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}

import io.circe.{Decoder, Encoder, Printer}

import scala.util.Try

trait FieldAdapter[T] {
  type DBTYPE
  def dbType: DbType[DBTYPE]
  def toDb(value: T): DBTYPE
  def fromDb(dbValue: DBTYPE): Either[ExtractorError, T]
}

trait PrimitiveFieldAdapter[T] extends FieldAdapter[T] {
  override def dbType: PrimitiveDbType[DBTYPE]
}

abstract class SimpleFieldAdapter[T](override val dbType: PrimitiveDbType[T]) extends PrimitiveFieldAdapter[T] {
  type DBTYPE = T
  def toDb(value: T): DBTYPE = value
  def fromDb(dbValue: DBTYPE): Either[ExtractorError, T] = Right(dbValue)
}

case object StringFieldAdapter extends SimpleFieldAdapter[String](StringDbType)

case object IntFieldAdapter extends SimpleFieldAdapter[Int](IntDbType)

case class OptionFieldAdapter[T, U](override val dbType: DbType[Option[U]], baseAdapter: FieldAdapter.Aux[T, U])
  extends FieldAdapter[Option[T]] {
  override type DBTYPE = Option[baseAdapter.DBTYPE]

  override def toDb(value: Option[T]): DBTYPE = value.map(baseAdapter.toDb)

  override def fromDb(dbValue: DBTYPE): Either[ExtractorError, Option[T]] = dbValue match {
    case None => Right(None)
    case Some(u) => baseAdapter.fromDb(u).map(Some(_))
  }
}

case class SeqFieldAdapter[T, U](override val dbType: PrimitiveDbType[Seq[U]], baseAdapter: FieldAdapter.Aux[T, U])
  extends PrimitiveFieldAdapter[Seq[T]] {
  import io.epifab.yadl.utils.EitherSupport._

  override type DBTYPE = Seq[U]

  override def toDb(value: Seq[T]): Seq[baseAdapter.DBTYPE] = value.map(baseAdapter.toDb)

  override def fromDb(dbValue: Seq[baseAdapter.DBTYPE]): Either[ExtractorError, Seq[T]] =
    firstLeftOrRights(dbValue.map(baseAdapter.fromDb))
}

object IntSeqFieldAdapter extends SeqFieldAdapter[Int, Int](IntSeqDbType, IntFieldAdapter)

object StringSeqFieldAdapter extends SeqFieldAdapter[String, String](StringSeqDbType, StringFieldAdapter)

case class Json[T](value: T)

case class JsonFieldAdapter[T]()(implicit decoder: Decoder[T], encoder: Encoder[T]) extends PrimitiveFieldAdapter[Json[T]] {
  import io.circe.parser.decode
  import io.circe.syntax._

  override type DBTYPE = String
  override val dbType: PrimitiveDbType[String] = StringDbType

  override def toDb(value: Json[T]): DBTYPE =
    value.value.asJson.pretty(Printer.noSpaces)

  override def fromDb(dbValue: String): Either[ExtractorError, Json[T]] =
    decode[T](dbValue) match {
      case Left(e) => Left(ExtractorError(e.getMessage))
      case Right(t) => Right(Json(t))
    }
}

//case object DateFieldAdapter extends PrimitiveFieldAdapter[LocalDate] {
//  override type DBTYPE = String
//  override def dbType: PrimitiveDbType[String] = StringDbType
//
//  override def toDb(value: LocalDate): String =
//    value.format(DateTimeFormatter.ISO_DATE)
//
//  override def fromDb(dbValue: String): Either[ExtractorError, LocalDate] =
//    Try(LocalDate.parse(dbValue, DateTimeFormatter.ISO_DATE))
//      .toEither
//      .left.map(error => ExtractorError(error.getMessage))
//}

case object DateTimeFieldAdapter extends PrimitiveFieldAdapter[LocalDateTime] {
  override type DBTYPE = String
  override def dbType: PrimitiveDbType[String] = StringDbType

  override def toDb(value: LocalDateTime): String =
    value.format(DateTimeFormatter.ISO_DATE_TIME)

  override def fromDb(dbValue: String): Either[ExtractorError, LocalDateTime] =
    Try(LocalDateTime.parse(dbValue, DateTimeFormatter.ofPattern("y-M-d H:m:s.S")))
      .toEither
      .left.map(error => ExtractorError(error.getMessage))
}

object FieldAdapter {
  type Aux[T, U] = FieldAdapter[T] { type DBTYPE = U }

  implicit val string: PrimitiveFieldAdapter[String] = StringFieldAdapter
  implicit val int: PrimitiveFieldAdapter[Int] = IntFieldAdapter
  implicit val stringSeq: PrimitiveFieldAdapter[Seq[String]] = StringSeqFieldAdapter
  implicit val intSeq: PrimitiveFieldAdapter[Seq[Int]] = IntSeqFieldAdapter

  implicit def option[T](implicit baseAdapter: PrimitiveFieldAdapter[T]): FieldAdapter[Option[T]] =
    OptionFieldAdapter[T, baseAdapter.DBTYPE](OptionDbType(baseAdapter.dbType), baseAdapter)

  implicit def json[T](implicit encoder: Encoder[T], decoder: Decoder[T]): PrimitiveFieldAdapter[Json[T]] =
    JsonFieldAdapter()

//  implicit val date: PrimitiveFieldAdapter[LocalDate] = DateFieldAdapter
  implicit val dateTime: PrimitiveFieldAdapter[LocalDateTime] = DateTimeFieldAdapter
}
