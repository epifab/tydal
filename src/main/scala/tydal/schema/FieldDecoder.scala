package tydal.schema

import java.time.{Instant, LocalDate, LocalDateTime, ZoneOffset}
import java.util.UUID

import io.circe.{Decoder => JsonDecoder}
import tydal.runtime.{DecoderError, SqlDate, SqlDateTime}
import tydal.utils.EitherSupport

import scala.util.Try

trait FieldDecoder[+T] { baseDecoder =>
  type DbType
  def dbType: FieldType[DbType]
  def decode(value: DbType): Either[DecoderError, T]

  def toSeq: FieldDecoder.Aux[Seq[T], Seq[DbType]] = new FieldDecoder[Seq[T]] {
    override type DbType = Seq[baseDecoder.DbType]
    override def dbType: FieldType[Seq[baseDecoder.DbType]] = baseDecoder.dbType.toSeq
    override def decode(value: DbType): Either[DecoderError, Seq[T]] = EitherSupport.firstLeftOrRights(value.map(baseDecoder.decode))
  }

  def toOption: FieldDecoder.Aux[Option[T], Option[DbType]] = new FieldDecoder[Option[T]] {
    override type DbType = Option[baseDecoder.DbType]
    override def dbType: FieldType[Option[baseDecoder.DbType]] = baseDecoder.dbType.toOption
    override def decode(value: DbType): Either[DecoderError, Option[T]] = value match {
      case None => Right(None)
      case Some(u) => baseDecoder.decode(u).map(Some(_))
    }
  }

  def map[U](f: T => Either[DecoderError, U]): FieldDecoder.Aux[U, DbType] = new FieldDecoder[U] {
    override type DbType = baseDecoder.DbType
    override def dbType: FieldType[DbType] = baseDecoder.dbType
    override def decode(value: DbType): Either[DecoderError, U] = baseDecoder.decode(value).flatMap(f)
  }
}

object FieldDecoder {
  type Aux[+T, D] = FieldDecoder[T] { type DbType = D }

  def apply[T](implicit decoder: FieldDecoder[T]): FieldDecoder[T] = decoder

  implicit val stringDecoder: FieldDecoder.Aux[String, String] = new FieldDecoder[String] {
    override type DbType = String
    override def dbType: FieldType[String] = TypeString
    override def decode(value: String): Either[DecoderError, String] = Right(value)
  }

  implicit val uuidDecoder: FieldDecoder.Aux[UUID, String] = new FieldDecoder[UUID] {
    override type DbType = String
    override def dbType: FieldType[String] = TypeUuid
    override def decode(value: String): Either[DecoderError, UUID] =
      Try(UUID.fromString(value)).toEither
        .left.map(ex => DecoderError(ex.getMessage))
  }

  implicit val intDecoder: FieldDecoder.Aux[Int, Int] = new FieldDecoder[Int] {
    override type DbType = Int
    override def dbType: FieldType[Int] = TypeInt
    override def decode(value: Int): Either[DecoderError, Int] = Right(value)
  }

  implicit val longDecoder: FieldDecoder.Aux[Long, Long] = new FieldDecoder[Long] {
    override type DbType = Long
    override def dbType: FieldType[Long] = TypeLong
    override def decode(value: Long): Either[DecoderError, Long] = Right(value)
  }

  implicit val doubleDecoder: FieldDecoder.Aux[Double, Double] = new FieldDecoder[Double] {
    override type DbType = Double
    override def dbType: FieldType[Double] = TypeDouble
    override def decode(value: Double): Either[DecoderError, Double] = Right(value)
  }

  implicit val instantDecoder: FieldDecoder.Aux[Instant, String] = new FieldDecoder[Instant] {
    override type DbType = String
    override def dbType: FieldType[String] = TypeDateTime
    override def decode(value: String): Either[DecoderError, Instant] =
      Try(LocalDateTime.parse(value, SqlDateTime.parser))
        .toEither
        .map(_.toInstant(ZoneOffset.UTC))
        .left.map(error => DecoderError(s"Could not parse timestamp: ${error.getMessage}"))
  }

  implicit val dateDecoder: FieldDecoder.Aux[LocalDate, String] = new FieldDecoder[LocalDate] {
    override type DbType = String
    override def dbType: FieldType[String] = TypeDate
    override def decode(value: String): Either[DecoderError, LocalDate] =
      Try(LocalDate.parse(value.take(10), SqlDate.formatter))
        .toEither
        .left.map(error => DecoderError(s"Could not parse date: ${error.getMessage}"))
  }

  def jsonDecoder[A](implicit jsonDecoder: JsonDecoder[A]): FieldDecoder.Aux[A, String] = new FieldDecoder[A] {
    import io.circe.parser.{decode => decodeJson}
    override type DbType = String
    override def dbType: FieldType[String] = TypeJson
    override def decode(value: String): Either[DecoderError, A] = decodeJson(value).left.map(circeError => DecoderError(circeError.getMessage))
  }

  def enumDecoder[A](sqlName: String, decodeFunction: String => Either[String, A]): FieldDecoder.Aux[A, String] = new FieldDecoder[A] {
    override type DbType = String
    override def dbType: FieldType[String] = TypeEnum(sqlName)
    override def decode(value: String): Either[DecoderError, A] = decodeFunction(value).left.map(DecoderError)
  }

  implicit def seqDecoder[T, U](implicit baseDecoder: FieldDecoder.Aux[T, U]): FieldDecoder.Aux[Seq[T], Seq[U]] =
    baseDecoder.toSeq

  implicit def optionDecoder[T, U](implicit baseDecoder: FieldDecoder.Aux[T, U]): FieldDecoder.Aux[Option[T], Option[U]] =
    baseDecoder.toOption
}
