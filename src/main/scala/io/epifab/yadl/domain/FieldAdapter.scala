package io.epifab.yadl.domain

import io.circe.{Decoder, Encoder, Printer}

trait FieldAdapter[T] {
  type DBTYPE
  def dbType: DbType[DBTYPE]
  def toDb(value: T): DBTYPE
  def fromDb(dbValue: DBTYPE): Either[ExtractorError, T]
}

abstract class PrimitiveFieldAdapter[T](val dbType: PrimitiveDbType[T]) extends FieldAdapter[T] {
  type DBTYPE = T
  def toDb(value: T): DBTYPE = value
  def fromDb(dbValue: DBTYPE): Either[ExtractorError, T] = Right(dbValue)
}

case object StringFieldAdapter extends PrimitiveFieldAdapter[String](StringDbType)

case object IntFieldAdapter extends PrimitiveFieldAdapter[Int](IntDbType)

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

case class Json[T](value: T)

case class JsonFieldAdapter[T](override val dbType: DbType[String])(implicit decoder: Decoder[T], encoder: Encoder[T]) extends FieldAdapter[Json[T]] {
  import io.circe.parser.decode
  import io.circe.syntax._

  type DBTYPE = String

  override def toDb(value: Json[T]): DBTYPE =
    value.value.asJson.pretty(Printer.noSpaces)

  override def fromDb(dbValue: String): Either[ExtractorError, Json[T]] =
    decode[T](dbValue) match {
      case Left(e) => Left(ExtractorError(e.getMessage))
      case Right(t) => Right(Json(t))
    }
}

object FieldAdapter {
  type Aux[T, U] = FieldAdapter[T] { type DBTYPE = U }

  implicit val string: PrimitiveFieldAdapter[String] = StringFieldAdapter

  implicit val int: PrimitiveFieldAdapter[Int] = IntFieldAdapter

  implicit def option[T](implicit baseAdapter: PrimitiveFieldAdapter[T]): FieldAdapter[Option[T]] = {
    OptionFieldAdapter(OptionDbType(baseAdapter.dbType), baseAdapter)
  }

  implicit def seq[T](implicit baseAdapter: PrimitiveFieldAdapter[T]): FieldAdapter[Seq[T]] =
    SeqFieldAdapter(SeqDbType(baseAdapter.dbType), baseAdapter)

  implicit def json[T](implicit stringDbType: DbType[String], encoder: Encoder[T], decoder: Decoder[T]): FieldAdapter[Json[T]] =
    JsonFieldAdapter(stringDbType)
}
