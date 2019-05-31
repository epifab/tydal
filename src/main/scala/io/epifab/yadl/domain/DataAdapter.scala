package io.epifab.yadl.domain

import java.time._
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}

import io.circe.{Decoder, Encoder}

import scala.util.Try

trait DataAdapter[T] {
  type DBTYPE
  def dbType: DbType[DBTYPE]
  def read(value: DBTYPE): Either[ExtractorError, T]
  def write(value: T): DBTYPE
}

trait FieldAdapter[T] extends DataAdapter[T] { outer =>
  type DBTYPE
  def dbType: ScalarDbType[DBTYPE]

  def imap[U](encode: U => T, decode: T => Either[ExtractorError, U]): FieldAdapter.Aux[U, DBTYPE] =
    new FieldAdapter[U] {
      override type DBTYPE = outer.DBTYPE
      override def dbType: ScalarDbType[outer.DBTYPE] = outer.dbType
      override def write(value: U): outer.DBTYPE = outer.write(encode(value))
      override def read(dbValue: outer.DBTYPE): Either[ExtractorError, U] = for {
        t <- outer.read(dbValue)
        u <- decode(t)
      } yield u
    }

  def imap[U](encode: U => T, decode: T => U)(implicit d1: DummyImplicit): FieldAdapter.Aux[U, DBTYPE] = {
    imap[U](
      encode,
      (dbValue: T) => Try(decode(dbValue))
        .toEither
        .left.map(error => ExtractorError(s"Extraction error: ${error.getMessage}", error))
    )
  }
}

class SimpleFieldAdapter[T](override val dbType: ScalarDbType[T]) extends FieldAdapter[T] {
  type DBTYPE = T
  def write(value: T): DBTYPE = value
  def read(dbValue: DBTYPE): Either[ExtractorError, T] = Right(dbValue)
}

case object StringFieldAdapter extends SimpleFieldAdapter[String](StringDbType)

case object IntFieldAdapter extends SimpleFieldAdapter[Int](IntDbType)

case object DoubleFieldAdapter extends SimpleFieldAdapter[Double](DoubleDbType)

case class OptionFieldAdapter[T, U](override val dbType: ScalarDbType[Option[U]], baseAdapter: FieldAdapter.Aux[T, U])
  extends FieldAdapter[Option[T]] {
  override type DBTYPE = Option[baseAdapter.DBTYPE]

  override def write(value: Option[T]): DBTYPE = value.map(baseAdapter.write)

  override def read(dbValue: DBTYPE): Either[ExtractorError, Option[T]] = dbValue match {
    case None => Right(None)
    case Some(u) => baseAdapter.read(u).map(Some(_))
  }
}

case class SeqFieldAdapter[T, U](override val dbType: ScalarDbType[Seq[U]], baseAdapter: FieldAdapter.Aux[T, U])
  extends FieldAdapter[Seq[T]] {
  import io.epifab.yadl.utils.EitherSupport._

  override type DBTYPE = Seq[U]

  override def write(value: Seq[T]): Seq[baseAdapter.DBTYPE] = value.map(baseAdapter.write)

  override def read(dbValue: Seq[baseAdapter.DBTYPE]): Either[ExtractorError, Seq[T]] =
    firstLeftOrRights(dbValue.map(baseAdapter.read))
}

class JsonFieldAdapter[T](implicit decoder: Decoder[T], encoder: Encoder[T]) extends FieldAdapter[T] {
  import io.circe.parser.decode
  import io.circe.syntax._

  override type DBTYPE = String

  override def dbType: JsonDbType.type = JsonDbType

  override def write(value: T): String =
    value.asJson.noSpaces

  override def read(dbValue: String): Either[ExtractorError, T] =
    decode[T](dbValue)
      .left.map(error => ExtractorError(s"Could not parse Json: ${error.getMessage}", error))
}

class EnumFieldAdapter[T](name: String, encode: T => String, decode: String => Either[ExtractorError, T]) extends FieldAdapter[T] {
  override type DBTYPE = String
  override def dbType: EnumDbType = EnumDbType(name)
  override def write(value: T): String = encode(value)
  override def read(dbValue: String): Either[ExtractorError, T] = decode(dbValue)
}

object DateFormatter {
  val formatter: DateTimeFormatter = new DateTimeFormatterBuilder()
    .appendPattern("yyyy-MM-dd")
    .toFormatter()
}

object DateFieldAdapter extends FieldAdapter[LocalDate] {
  override type DBTYPE = String
  override def dbType: DateDbType.type = DateDbType

  override def write(value: LocalDate): String =
    value.format(DateFormatter.formatter)

  override def read(dbValue: String): Either[ExtractorError, LocalDate] =
    Try(LocalDate.parse(dbValue.take(10), DateFormatter.formatter))
      .toEither
      .left.map(error => ExtractorError(s"Could not parse date: ${error.getMessage}", error))
}

object TimestampFormatter {
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
    .appendPattern("[.SSSSSSSSS][.SSSSSS][.SSS][.S]")
    .toFormatter()
}

object DateTimeFieldAdapter extends FieldAdapter[LocalDateTime] {
  override type DBTYPE = String
  override def dbType: DateTimeDbType.type = DateTimeDbType

  override def write(value: LocalDateTime): String =
    value.format(TimestampFormatter.formatter)

  override def read(dbValue: String): Either[ExtractorError, LocalDateTime] =
    Try(LocalDateTime.parse(dbValue, TimestampFormatter.parser))
      .toEither
      .left.map(error => ExtractorError(s"Could not parse timestamp: ${error.getMessage}", error))
}

object InstantFieldAdapter extends FieldAdapter[Instant] {
  override type DBTYPE = String
  override def dbType: DateTimeDbType.type = DateTimeDbType

  override def write(value: Instant): String =
    LocalDateTime.ofInstant(value, ZoneOffset.UTC).format(TimestampFormatter.formatter)

  override def read(dbValue: String): Either[ExtractorError, Instant] =
    Try(LocalDateTime.parse(dbValue, TimestampFormatter.parser))
      .toEither
      .map(_.toInstant(ZoneOffset.UTC))
      .left.map(error => ExtractorError(s"Could not parse timestamp: ${error.getMessage}", error))
}

object StringSeqFieldAdapter extends SeqFieldAdapter[String, String](StringSeqDbType, StringFieldAdapter)
object IntSeqFieldAdapter extends SeqFieldAdapter[Int, Int](IntSeqDbType, IntFieldAdapter)
object DoubleSeqFieldAdapter extends SeqFieldAdapter[Double, Double](DoubleSeqDbType, DoubleFieldAdapter)
object DateSeqFieldAdapter extends SeqFieldAdapter[LocalDate, String](DateSeqDbType, DateFieldAdapter)
object DateTimeSeqFieldAdapter extends SeqFieldAdapter[LocalDateTime, String](DateTimeSeqDbType, DateTimeFieldAdapter)
object InstantSeqFieldAdapter extends SeqFieldAdapter[Instant, String](DateTimeSeqDbType, InstantFieldAdapter)


object FieldAdapter {
  type Aux[T, D] = FieldAdapter[T] { type DBTYPE = D }

  implicit val string: FieldAdapter.Aux[String, String] = StringFieldAdapter
  implicit val int: FieldAdapter.Aux[Int, Int] = IntFieldAdapter
  implicit val double: FieldAdapter.Aux[Double, Double] = DoubleFieldAdapter
  implicit val date: FieldAdapter.Aux[LocalDate, String] = DateFieldAdapter
  implicit val dateTime: FieldAdapter.Aux[LocalDateTime, String] = DateTimeFieldAdapter
  implicit val instant: FieldAdapter.Aux[Instant, String] = InstantFieldAdapter
  implicit val geometry: FieldAdapter.Aux[Geometry, String] =
    new SimpleFieldAdapter(GeometryDbType).imap(
      (geometry: Geometry) => geometry.value,
      (value: String) => Geometry(value)
    )
  implicit val geography: FieldAdapter.Aux[Geography, String] =
    new SimpleFieldAdapter(GeographyDbType).imap(
      (geography: Geography) => geography.value,
      (value: String) => Geography(value)
    )

  def enum[T](name: String, encode: T => String, decode: String => T): EnumFieldAdapter[T] =
    new EnumFieldAdapter[T](name, encode, v => Right(decode(v)))

  implicit val stringSeq: FieldAdapter.Aux[Seq[String], Seq[String]] = StringSeqFieldAdapter
  implicit val intSeq: FieldAdapter.Aux[Seq[Int], Seq[Int]] = IntSeqFieldAdapter
  implicit val doubleSeq: FieldAdapter.Aux[Seq[Double], Seq[Double]] = DoubleSeqFieldAdapter
  implicit val dateSeq: FieldAdapter.Aux[Seq[LocalDate], Seq[String]] = DateSeqFieldAdapter
  implicit val dateTimeSeq: FieldAdapter.Aux[Seq[LocalDateTime], Seq[String]] = DateTimeSeqFieldAdapter
  implicit val instantSeq: FieldAdapter.Aux[Seq[Instant], Seq[String]] = InstantSeqFieldAdapter
  implicit def enumSeq[T](implicit enum: EnumFieldAdapter[T]): FieldAdapter[Seq[T]] =
    new SeqFieldAdapter[T, String](EnumSeqDbType(enum.dbType), enum)

  implicit def option[T, U](implicit baseAdapter: FieldAdapter.Aux[T, U]): FieldAdapter.Aux[Option[T], Option[U]] =
    OptionFieldAdapter[T, baseAdapter.DBTYPE](OptionDbType(baseAdapter.dbType), baseAdapter)

  def json[T](implicit encoder: Encoder[T], decoder: Decoder[T]): FieldAdapter[T] =
    new JsonFieldAdapter[T]
}

object DataAdapter {
  implicit def field[T](implicit adapter: FieldAdapter[T]): DataAdapter[T] = adapter
}
