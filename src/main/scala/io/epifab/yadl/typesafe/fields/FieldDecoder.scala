package io.epifab.yadl.typesafe.fields

import scala.language.reflectiveCalls

case class DecoderError(reason: String)

trait FieldDecoder[+T] {
  type DBTYPE
  def dbType: FieldType[DBTYPE]
  def decode(value: DBTYPE): Either[DecoderError, T]
}

object FieldDecoder {
  type Aux[+T, D] = FieldDecoder[T] { type DBTYPE = D }

  case class OptionFieldDecoder[T, U](override val dbType: FieldType[Option[U]], baseDecoder: FieldDecoder.Aux[T, U]) extends FieldDecoder[Option[T]] {
    override type DBTYPE = Option[baseDecoder.DBTYPE]
    override def decode(dbValue: DBTYPE): Either[DecoderError, Option[T]] = dbValue match {
      case None => Right(None)
      case Some(u) => baseDecoder.decode(u).map(Some(_))
    }
  }

  implicit val stringDecoder: FieldDecoder.Aux[String, String] = new FieldDecoder[String] {
    override type DBTYPE = String
    override def dbType: FieldType[String] = TypeString
    override def decode(value: String): Either[DecoderError, String] = Right(value)
  }

  implicit val intDecoder: FieldDecoder.Aux[Int, Int] = new FieldDecoder[Int] {
    override type DBTYPE = Int
    override def dbType: FieldType[Int] = TypeInt
    override def decode(value: Int): Either[DecoderError, Int] = Right(value)
  }

  implicit def optionDecoder[T, U](implicit baseDecoder: FieldDecoder.Aux[T, U]): FieldDecoder.Aux[Option[T], Option[U]] =
    new OptionFieldDecoder[T, U](TypeOption(baseDecoder.dbType), baseDecoder)
}
