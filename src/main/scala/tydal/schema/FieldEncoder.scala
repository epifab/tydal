package tydal.schema

import java.time.{Instant, LocalDate, LocalDateTime, ZoneOffset}
import java.util.UUID

import io.circe.{Encoder => JsonEncoder}
import tydal.schema
import tydal.runtime.{DecoderError, SqlDate, SqlDateTime}

trait FieldEncoder[-T] { baseEncoder =>
  type DbType
  def dbType: FieldType[DbType]
  def encode(value: T): DbType

  def toSeq: schema.FieldEncoder.Aux[Seq[T], Seq[DbType]] = new FieldEncoder[Seq[T]] {
    override type DbType = Seq[baseEncoder.DbType]
    override def dbType: FieldType[Seq[baseEncoder.DbType]] = baseEncoder.dbType.toSeq
    override def encode(value: Seq[T]): DbType = value.map(baseEncoder.encode)
  }

  def toOption: FieldEncoder.Aux[Option[T], Option[DbType]] = new FieldEncoder[Option[T]] {
    override type DbType = Option[baseEncoder.DbType]
    override def dbType: FieldType[Option[baseEncoder.DbType]] = baseEncoder.dbType.toOption
    override def encode(value: Option[T]): DbType = value.map(baseEncoder.encode)
  }

  def contramap[U](f: U => T): schema.FieldEncoder.Aux[U, DbType] = new FieldEncoder[U] {
    override type DbType = baseEncoder.DbType
    override def dbType: FieldType[DbType] = baseEncoder.dbType
    override def encode(value: U): FieldEncoder.this.DbType = baseEncoder.encode(f(value))
  }
}

object FieldEncoder {
  type Aux[-T, D] = FieldEncoder[T] { type DbType = D }

  def apply[T](implicit decoder: FieldDecoder[T]): FieldDecoder[T] = decoder

  implicit val stringEncoder: FieldEncoder.Aux[String, String] = new FieldEncoder[String] {
    override type DbType = String
    override def dbType: FieldType[String] = TypeString
    override def encode(value: String): String = value
  }

  implicit val uuidEncoder: FieldEncoder.Aux[UUID, String] =
    stringEncoder.contramap(_.toString)

  implicit val intEncoder: FieldEncoder.Aux[Int, Int] = new FieldEncoder[Int] {
    override type DbType = Int
    override def dbType: FieldType[Int] = TypeInt
    override def encode(value: Int): Int = value
  }

  implicit val longEncoder: FieldEncoder.Aux[Long, Long] = new FieldEncoder[Long] {
    override type DbType = Long
    override def dbType: FieldType[Long] = TypeLong
    override def encode(value: Long): Long = value
  }

  implicit val doubleEncoder: FieldEncoder.Aux[Double, Double] = new FieldEncoder[Double] {
    override type DbType = Double
    override def dbType: FieldType[Double] = TypeDouble
    override def encode(value: Double): Double = value
  }

  implicit val instantEncoder: FieldEncoder.Aux[Instant, String] = new FieldEncoder[Instant] {
    override type DbType = String
    override def dbType: FieldType[String] = TypeDateTime
    override def encode(value: Instant): String = LocalDateTime.ofInstant(value, ZoneOffset.UTC).format(SqlDateTime.formatter)
  }

  implicit val dateEncoder: FieldEncoder.Aux[LocalDate, String] = new FieldEncoder[LocalDate] {
    override type DbType = String
    override def dbType: FieldType[String] = TypeDate
    override def encode(value: LocalDate): String = value.format(SqlDate.formatter)
  }

  def jsonEncoder[A](implicit jsonEncoder: JsonEncoder[A]): FieldEncoder.Aux[A, String] = new FieldEncoder[A] {
    override type DbType = String
    override def dbType: FieldType[String] = TypeJson
    override def encode(value: A): String = jsonEncoder(value).noSpaces
  }

  def enumEncoder[A](sqlName: String, encodeFunction: A => String): FieldEncoder.Aux[A, String] = new FieldEncoder[A] {
    override type DbType = String
    override def dbType: FieldType[String] = TypeEnum(sqlName)
    override def encode(value: A): String = encodeFunction(value)
  }

  implicit def seqEncoder[T, U](implicit baseEncoder: FieldEncoder.Aux[T, U]): FieldEncoder.Aux[Seq[T], Seq[U]] =
    baseEncoder.toSeq

  implicit def optionEncoder[T, U](implicit baseEncoder: FieldEncoder.Aux[T, U]): FieldEncoder.Aux[Option[T], Option[U]] =
    baseEncoder.toOption
}
