package io.epifab.yadl.domain

import io.circe.{Decoder, Encoder, Printer}

object FieldTypes {
  sealed trait DbType[T]

  sealed trait PrimitiveDbType[T] extends DbType[T]

  case object IntDbType extends PrimitiveDbType[Int]
  case object StringDbType extends PrimitiveDbType[String]
  case class OptionDbType[T](dbType: DbType[T]) extends DbType[Option[T]]
  case class SeqDbType[T](dbType: DbType[T]) extends DbType[Seq[T]]

  object DbType {
    implicit val int: PrimitiveDbType[Int] = IntDbType
    implicit val string: PrimitiveDbType[String] = StringDbType
    implicit def option[T](implicit dbType: PrimitiveDbType[T]): DbType[Option[T]] =
      OptionDbType(dbType)
    implicit def seq[T](implicit dbType: PrimitiveDbType[T]): DbType[Seq[T]] =
      SeqDbType(dbType)
  }

  trait FieldAdapter[T] {
    type DBTYPE
    def dbType: DbType[DBTYPE]
    def toDb(value: T): DBTYPE
    def fromDb(dbValue: DBTYPE): Either[Error, T]
  }

  abstract class SimpleFieldAdapter[T](val dbType: DbType[T]) extends FieldAdapter[T] {
    type DBTYPE = T
    def toDb(value: T): DBTYPE = value
    def fromDb(dbValue: DBTYPE): Either[Error, T] = Right(dbValue)
  }

  case object StringFieldAdapter extends SimpleFieldAdapter[String](StringDbType)
  case object IntFieldAdapter extends SimpleFieldAdapter[Int](IntDbType)
  case class OptionFieldAdapter[T](override val dbType: DbType[Option[T]]) extends SimpleFieldAdapter(dbType)
  case class SeqFieldAdapter[T](override val dbType: DbType[Seq[T]]) extends SimpleFieldAdapter(dbType)

  case class Json[T](t: T)

  case class JsonFieldAdapter[T](override val dbType: DbType[String])(implicit decoder: Decoder[T], encoder: Encoder[T]) extends FieldAdapter[Json[T]] {
    import io.circe.parser.decode
    import io.circe.syntax._

    type DBTYPE = String

    override def toDb(value: Json[T]): DBTYPE =
      value.t.asJson.pretty(Printer.noSpaces)

    override def fromDb(dbValue: String): Either[Error, Json[T]] =
      decode[T](dbValue) match {
        case Left(e) => Left(ExtractorError(e.getMessage))
        case Right(t) => Right(Json(t))
      }
  }

  object FieldAdapter {
    implicit val string: FieldAdapter[String] = StringFieldAdapter
    implicit val int: FieldAdapter[Int] = IntFieldAdapter
    implicit def option[T](implicit dbType: DbType[Option[T]]): FieldAdapter[Option[T]] =
      OptionFieldAdapter[T](dbType)
    implicit def seq[T](implicit dbType: DbType[Seq[T]]): FieldAdapter[Seq[T]] =
      SeqFieldAdapter(dbType)
    implicit def json[T](implicit stringDbType: DbType[String], encoder: Encoder[T], decoder: Decoder[T]): FieldAdapter[Json[T]] =
      JsonFieldAdapter(stringDbType)
  }
}
