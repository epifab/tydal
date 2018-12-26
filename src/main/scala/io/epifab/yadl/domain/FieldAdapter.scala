package io.epifab.yadl.domain

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}

import io.circe.{Decoder, Encoder, Printer}

import scala.util.Try

trait FieldAdapter[T] { outer =>
  type DBTYPE
  def dbType: DbType[DBTYPE]
  def toDb(value: T): DBTYPE
  def fromDb(dbValue: DBTYPE): Either[ExtractorError, T]

  def bimap[U](encode: U => T, decode: T => Either[ExtractorError, U]): FieldAdapter[U] =
    new FieldAdapter[U] {
      override type DBTYPE = outer.DBTYPE
      override def dbType: DbType[outer.DBTYPE] = outer.dbType
      override def toDb(value: U): outer.DBTYPE = outer.toDb(encode(value))
      override def fromDb(dbValue: outer.DBTYPE): Either[ExtractorError, U] = for {
        t <- outer.fromDb(dbValue)
        u <- decode(t)
      } yield u
    }

  def bimap[U](encode: U => T, decode: T => U)(implicit d1: DummyImplicit): FieldAdapter[U] = {
    bimap[U](
      encode,
      (dbValue: T) => Try(decode(dbValue))
        .toEither
        .left.map(error => ExtractorError(error.getMessage))
    )
  }

  def bimap[U](encode: U => T, decode: T => Option[U])(implicit d1: DummyImplicit, d2: DummyImplicit): FieldAdapter[U] = {
    bimap[U](
      encode,
      (dbValue: T) => decode(dbValue) match {
        case Some(value) => Right(value)
        case None => Left(ExtractorError(s"Could not decode $dbValue"))
      }
    )
  }
}

abstract class SimpleFieldAdapter[T](override val dbType: DbType[T]) extends FieldAdapter[T] {
  type DBTYPE = T
  def toDb(value: T): DBTYPE = value
  def fromDb(dbValue: DBTYPE): Either[ExtractorError, T] = Right(dbValue)
}

case object StringFieldAdapter extends SimpleFieldAdapter[String](StringDbType)

case object IntFieldAdapter extends SimpleFieldAdapter[Int](IntDbType)

case object DoubleFieldAdapter extends SimpleFieldAdapter[Double](DoubleDbType)

case class OptionFieldAdapter[T, U](override val dbType: DbType[Option[U]], baseAdapter: FieldAdapter.Aux[T, U])
  extends FieldAdapter[Option[T]] {
  override type DBTYPE = Option[baseAdapter.DBTYPE]

  override def toDb(value: Option[T]): DBTYPE = value.map(baseAdapter.toDb)

  override def fromDb(dbValue: DBTYPE): Either[ExtractorError, Option[T]] = dbValue match {
    case None => Right(None)
    case Some(u) => baseAdapter.fromDb(u).map(Some(_))
  }
}

case class SeqFieldAdapter[T, U](override val dbType: DbType[Seq[U]], baseAdapter: FieldAdapter.Aux[T, U])
  extends FieldAdapter[Seq[T]] {
  import io.epifab.yadl.utils.EitherSupport._

  override type DBTYPE = Seq[U]

  override def toDb(value: Seq[T]): Seq[baseAdapter.DBTYPE] = value.map(baseAdapter.toDb)

  override def fromDb(dbValue: Seq[baseAdapter.DBTYPE]): Either[ExtractorError, Seq[T]] =
    firstLeftOrRights(dbValue.map(baseAdapter.fromDb))
}

class JsonFieldAdapter[T](implicit decoder: Decoder[T], encoder: Encoder[T]) extends FieldAdapter[T] {
  import io.circe.parser.decode
  import io.circe.syntax._

  override type DBTYPE = String

  override def dbType: DbType[String] = JsonDbType

  override def toDb(value: T): String =
    value.asJson.noSpaces

  override def fromDb(dbValue: String): Either[ExtractorError, T] =
    decode[T](dbValue)
      .left.map(error => ExtractorError(error.getMessage))
}

class EnumFieldAdapter[T](name: String, encode: T => String, decode: String => Either[ExtractorError, T]) extends FieldAdapter[T] {
  override type DBTYPE = String
  override def dbType: EnumDbType = EnumDbType(name)
  override def toDb(value: T): String = encode(value)
  override def fromDb(dbValue: String): Either[ExtractorError, T] = decode(dbValue)
}

object DateFieldAdapter extends FieldAdapter[LocalDate] {
  override type DBTYPE = String
  override def dbType: DateDbType.type = DateDbType

  override def toDb(value: LocalDate): String =
    value.format(DateTimeFormatter.ISO_DATE)

  override def fromDb(dbValue: String): Either[ExtractorError, LocalDate] =
    Try(LocalDate.parse(dbValue.take(10), DateTimeFormatter.ofPattern("yyyy-MM-dd")))
      .toEither
      .left.map(error => ExtractorError(error.getMessage))
}

object DateTimeFieldAdapter extends FieldAdapter[LocalDateTime] {
  override type DBTYPE = String
  override def dbType: DbType[String] = DateTimeDbType

  override def toDb(value: LocalDateTime): String =
    value.format(DateTimeFormatter.ISO_DATE_TIME)

  override def fromDb(dbValue: String): Either[ExtractorError, LocalDateTime] =
    Try(LocalDateTime.parse(dbValue, DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.S")))
      .toEither
      .left.map(error => ExtractorError(error.getMessage))
}

object StringSeqFieldAdapter extends SeqFieldAdapter[String, String](StringSeqDbType, StringFieldAdapter)
object IntSeqFieldAdapter extends SeqFieldAdapter[Int, Int](IntSeqDbType, IntFieldAdapter)
object DoubleSeqFieldAdapter extends SeqFieldAdapter[Double, Double](DoubleSeqDbType, DoubleFieldAdapter)
object DateSeqFieldAdapter extends SeqFieldAdapter[LocalDate, String](DateSeqDbType, DateFieldAdapter)
object DateTimeSeqFieldAdapter extends SeqFieldAdapter[LocalDateTime, String](DateTimeSeqDbType, DateTimeFieldAdapter)

object FieldAdapter {
  type Aux[T, U] = FieldAdapter[T] { type DBTYPE = U }

  implicit val string: FieldAdapter[String] = StringFieldAdapter
  implicit val int: FieldAdapter[Int] = IntFieldAdapter
  implicit val double: FieldAdapter[Double] = DoubleFieldAdapter
  implicit val date: FieldAdapter[LocalDate] = DateFieldAdapter
  implicit val dateTime: FieldAdapter[LocalDateTime] = DateTimeFieldAdapter
  def enum[T](name: String, encode: T => String, decode: String => T): EnumFieldAdapter[T] =
    new EnumFieldAdapter[T](name, encode, v => Right(decode(v)))

  implicit val stringSeq: FieldAdapter[Seq[String]] = StringSeqFieldAdapter
  implicit val intSeq: FieldAdapter[Seq[Int]] = IntSeqFieldAdapter
  implicit val doubleSeq: FieldAdapter[Seq[Double]] = DoubleSeqFieldAdapter
  implicit val dateSeq: FieldAdapter[Seq[LocalDate]] = DateSeqFieldAdapter
  implicit val dateTimeSeq: FieldAdapter[Seq[LocalDateTime]] = DateTimeSeqFieldAdapter
  implicit def enumSeq[T](implicit enum: EnumFieldAdapter[T]): FieldAdapter[Seq[T]] =
    new SeqFieldAdapter[T, String](EnumSeqDbType(enum.dbType), enum)

  implicit def option[T](implicit baseAdapter: FieldAdapter[T]): FieldAdapter[Option[T]] =
    OptionFieldAdapter[T, baseAdapter.DBTYPE](OptionDbType(baseAdapter.dbType), baseAdapter)

  def json[T](implicit encoder: Encoder[T], decoder: Decoder[T]): FieldAdapter[T] =
    new JsonFieldAdapter[T]

}
