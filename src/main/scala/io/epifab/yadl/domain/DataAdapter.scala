package io.epifab.yadl.domain

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter

import io.circe.{Decoder, Encoder}
import shapeless.{::, HList, HNil}

import scala.util.Try

sealed trait DataAdapter[T] {
  type DBTYPE
  def dbType: DbType[DBTYPE]
  def fromDb(value: DBTYPE): Either[ExtractorError, T]
  def toDb(value: T): DBTYPE
}

sealed trait FieldAdapter[T] extends DataAdapter[T] {
  type DBTYPE
  def dbType: ScalarDbType[DBTYPE]
}

sealed trait FieldsAdapter[T] extends DataAdapter[T] {
  type DBTYPE <: HList
  def dbType: CompositeDbType[DBTYPE]
}

abstract class SimpleFieldAdapter[T](override val dbType: ScalarDbType[T]) extends FieldAdapter[T] {
  type DBTYPE = T
  def toDb(value: T): DBTYPE = value
  def fromDb(dbValue: DBTYPE): Either[ExtractorError, T] = Right(dbValue)
}

case object StringFieldAdapter extends SimpleFieldAdapter[String](StringDbType)

case object IntFieldAdapter extends SimpleFieldAdapter[Int](IntDbType)

case object DoubleFieldAdapter extends SimpleFieldAdapter[Double](DoubleDbType)

case class OptionFieldAdapter[T, U](override val dbType: ScalarDbType[Option[U]], baseAdapter: FieldAdapter.Aux[T, U])
  extends FieldAdapter[Option[T]] {
  override type DBTYPE = Option[baseAdapter.DBTYPE]

  override def toDb(value: Option[T]): DBTYPE = value.map(baseAdapter.toDb)

  override def fromDb(dbValue: DBTYPE): Either[ExtractorError, Option[T]] = dbValue match {
    case None => Right(None)
    case Some(u) => baseAdapter.fromDb(u).map(Some(_))
  }
}

case class SeqFieldAdapter[T, U](override val dbType: ScalarDbType[Seq[U]], baseAdapter: FieldAdapter.Aux[T, U])
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

  override def dbType: JsonDbType.type = JsonDbType

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
  override def dbType: DateTimeDbType.type = DateTimeDbType

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
  type Aux[T, D] = FieldAdapter[T] { type DBTYPE = D }

  implicit val string: FieldAdapter.Aux[String, String] = StringFieldAdapter
  implicit val int: FieldAdapter.Aux[Int, Int] = IntFieldAdapter
  implicit val double: FieldAdapter.Aux[Double, Double] = DoubleFieldAdapter
  implicit val date: FieldAdapter.Aux[LocalDate, String] = DateFieldAdapter
  implicit val dateTime: FieldAdapter.Aux[LocalDateTime, String] = DateTimeFieldAdapter
  def enum[T](name: String, encode: T => String, decode: String => T): EnumFieldAdapter[T] =
    new EnumFieldAdapter[T](name, encode, v => Right(decode(v)))

  implicit val stringSeq: FieldAdapter.Aux[Seq[String], Seq[String]] = StringSeqFieldAdapter
  implicit val intSeq: FieldAdapter.Aux[Seq[Int], Seq[Int]] = IntSeqFieldAdapter
  implicit val doubleSeq: FieldAdapter.Aux[Seq[Double], Seq[Double]] = DoubleSeqFieldAdapter
  implicit val dateSeq: FieldAdapter.Aux[Seq[LocalDate], Seq[String]] = DateSeqFieldAdapter
  implicit val dateTimeSeq: FieldAdapter.Aux[Seq[LocalDateTime], Seq[String]] = DateTimeSeqFieldAdapter
  implicit def enumSeq[T](implicit enum: EnumFieldAdapter[T]): FieldAdapter[Seq[T]] =
    new SeqFieldAdapter[T, String](EnumSeqDbType(enum.dbType), enum)

  implicit def option[T, U](implicit baseAdapter: FieldAdapter.Aux[T, U]): FieldAdapter.Aux[Option[T], Option[U]] =
    OptionFieldAdapter[T, baseAdapter.DBTYPE](OptionDbType(baseAdapter.dbType), baseAdapter)

  def json[T](implicit encoder: Encoder[T], decoder: Decoder[T]): FieldAdapter[T] =
    new JsonFieldAdapter[T]
}

object FieldsAdapter {
  implicit val empty: FieldsAdapter[HNil] = new FieldsAdapter[HNil] {
    override type DBTYPE = HNil
    override def dbType: CompositeDbType[HNil] = implicitly
    override def fromDb(value: HNil): Either[ExtractorError, HNil] = Right(HNil)
    override def toDb(value: HNil): HNil = HNil
  }

  implicit def nonEmpty[H, T <: HList](implicit head: FieldAdapter[H], tail: FieldsAdapter[T]): FieldsAdapter[H :: T] = new FieldsAdapter[H :: T] {
    override type DBTYPE = head.DBTYPE :: tail.DBTYPE
    override def dbType: CompositeDbType[head.DBTYPE :: tail.DBTYPE] = CompositeDbType.nonEmpty(head.dbType, tail.dbType)

    override def fromDb(value: DBTYPE): Either[ExtractorError, H :: T] =
      head.fromDb(value.head).flatMap { h => tail.fromDb(value.tail).map(t => h :: t) }
    override def toDb(value: H :: T): DBTYPE =
      head.toDb(value.head) :: tail.toDb(value.tail)
  }
}

object DataAdapter {
  implicit def field[T](implicit adapter: FieldAdapter[T]): DataAdapter[T] = adapter
  implicit def fields[T](implicit adapter: FieldsAdapter[T]): DataAdapter[T] = adapter
}
