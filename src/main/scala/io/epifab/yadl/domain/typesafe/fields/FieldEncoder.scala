package io.epifab.yadl.domain.typesafe.fields

import scala.language.reflectiveCalls

trait FieldEncoder[-T] {
  type DBTYPE
  def dbType: ScalarDbType[DBTYPE]
  def encode(value: T): DBTYPE
}

object FieldEncoder {
  type Aux[-T, D] = FieldEncoder[T] { type DBTYPE = D }

  case class OptionFieldEncoder[T, U](override val dbType: ScalarDbType[Option[U]], baseEncoder: FieldEncoder.Aux[T, U]) extends FieldEncoder[Option[T]] {
    override type DBTYPE = Option[baseEncoder.DBTYPE]
    override def encode(value: Option[T]): Option[U] = value.map(baseEncoder.encode)
  }

  implicit val stringEncoder: FieldEncoder.Aux[String, String] = new FieldEncoder[String] {
    override type DBTYPE = String
    override def dbType: ScalarDbType[String] = StringDbType
    override def encode(value: String): String = value
  }

  implicit val intEncoder: FieldEncoder.Aux[Int, Int] = new FieldEncoder[Int] {
    override type DBTYPE = Int
    override def dbType: ScalarDbType[Int] = IntDbType
    override def encode(value: Int): Int = value
  }

  implicit def optionEncoder[T, U](implicit baseEncoder: FieldEncoder.Aux[T, U]): FieldEncoder.Aux[Option[T], Option[U]] =
    new OptionFieldEncoder[T, U](OptionDbType(baseEncoder.dbType), baseEncoder)
}
