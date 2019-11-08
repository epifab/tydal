package io.epifab.tydal.fields

import java.time.{Instant, LocalDate, LocalDateTime, ZoneOffset}

import io.circe.{Encoder => JsonEncoder}
import io.epifab.tydal.fields
import io.epifab.tydal.runner.{SqlDate, SqlDateTime}

trait FieldEncoder[-T] { baseEncoder =>
  type DBTYPE
  def dbType: FieldType[DBTYPE]
  def encode(value: T): DBTYPE

  def toSeq: fields.FieldEncoder.Aux[Seq[T], Seq[DBTYPE]] = new FieldEncoder[Seq[T]] {
    override type DBTYPE = Seq[baseEncoder.DBTYPE]
    override def dbType: FieldType[Seq[baseEncoder.DBTYPE]] = baseEncoder.dbType.toSeq
    override def encode(value: Seq[T]): DBTYPE = value.map(baseEncoder.encode)
  }

  def toOption: FieldEncoder.Aux[Option[T], Option[DBTYPE]] = new FieldEncoder[Option[T]] {
    override type DBTYPE = Option[baseEncoder.DBTYPE]
    override def dbType: FieldType[Option[baseEncoder.DBTYPE]] = baseEncoder.dbType.toOption
    override def encode(value: Option[T]): DBTYPE = value.map(baseEncoder.encode)
  }
}

object FieldEncoder {
  type Aux[-T, D] = FieldEncoder[T] { type DBTYPE = D }

  implicit val stringEncoder: FieldEncoder.Aux[String, String] = new FieldEncoder[String] {
    override type DBTYPE = String
    override def dbType: FieldType[String] = TypeString
    override def encode(value: String): String = value
  }

  implicit val intEncoder: FieldEncoder.Aux[Int, Int] = new FieldEncoder[Int] {
    override type DBTYPE = Int
    override def dbType: FieldType[Int] = TypeInt
    override def encode(value: Int): Int = value
  }

  implicit val doubleEncoder: FieldEncoder.Aux[Double, Double] = new FieldEncoder[Double] {
    override type DBTYPE = Double
    override def dbType: FieldType[Double] = TypeDouble
    override def encode(value: Double): Double = value
  }

  implicit val instantEncoder: FieldEncoder.Aux[Instant, String] = new FieldEncoder[Instant] {
    override type DBTYPE = String
    override def dbType: FieldType[String] = TypeDateTime
    override def encode(value: Instant): String = LocalDateTime.ofInstant(value, ZoneOffset.UTC).format(SqlDateTime.formatter)
  }

  implicit val dateEncoder: FieldEncoder.Aux[LocalDate, String] = new FieldEncoder[LocalDate] {
    override type DBTYPE = String
    override def dbType: FieldType[String] = TypeDate
    override def encode(value: LocalDate): String = value.format(SqlDate.formatter)
  }

  def jsonEncoder[A](implicit jsonEncoder: JsonEncoder[A]): FieldEncoder.Aux[A, String] = new FieldEncoder[A] {
    override type DBTYPE = String
    override def dbType: FieldType[String] = TypeJson
    override def encode(value: A): String = jsonEncoder(value).noSpaces
  }

  def enumEncoder[A](sqlName: String, encodeFunction: A => String): FieldEncoder.Aux[A, String] = new FieldEncoder[A] {
    override type DBTYPE = String
    override def dbType: FieldType[String] = TypeEnum(sqlName)
    override def encode(value: A): String = encodeFunction(value)
  }

  implicit def seqEncoder[T, U](implicit baseEncoder: FieldEncoder.Aux[T, U]): FieldEncoder.Aux[Seq[T], Seq[U]] =
    baseEncoder.toSeq

  implicit def optionEncoder[T, U](implicit baseEncoder: FieldEncoder.Aux[T, U]): FieldEncoder.Aux[Option[T], Option[U]] =
    baseEncoder.toOption
}
