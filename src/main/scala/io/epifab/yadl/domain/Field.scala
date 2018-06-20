package io.epifab.yadl.domain

import io.circe.{Decoder, Encoder, Printer}
import io.epifab.yadl.utils.EitherSupport._

import scala.language.implicitConversions
import scala.util.Try

sealed trait DbFieldType[T]

object DbFieldType {
  sealed trait ScalarDbType[T] extends DbFieldType[T]
  implicit final case object StringDbType extends ScalarDbType[String]
  implicit final case object IntDbType extends ScalarDbType[Int]
  implicit final case object ArrayDbType extends DbFieldType[java.sql.Array]
}

trait NullableField[-T] {
  def nullValue[U <: T]: U
}

object NullableField {
  implicit val nullableString: NullableField[Object] = null
}

trait FieldAdapter[T, U] {
  implicit def dbType: DbFieldType[U]

  def extract(u: U): Either[ExtractorError, T]
  def inject(t: T): U
}

object FieldAdapter {
  abstract class SimpleFieldAdapter[T](implicit val dbType: DbFieldType[T]) extends FieldAdapter[T, T] {
    override def extract(u: T): Either[ExtractorError, T] = Right(u)
    override def inject(t: T): T = t
  }

  implicit final case object StringFieldAdapter extends SimpleFieldAdapter[String]
  implicit final case object IntFieldAdapter extends SimpleFieldAdapter[Int]

  case class Json[T](value: T)

  implicit def jsonFieldAdapter[T](implicit
    dbType: DbFieldType[String],
    stringAdapter: FieldAdapter[String, String],
    decoder: Decoder[T],
    encoder: Encoder[T]
  ): FieldAdapter[Json[T], String] = new FieldAdapter[Json[T], String] {

    import io.circe.parser.decode
    import io.circe.syntax._

    override def extract(u: String): Either[ExtractorError, Json[T]] = stringAdapter.extract(u)
      .flatMap { json =>
        decode(json) match {
          case Left(e) => Left(ExtractorError(e.getMessage))
          case Right(t) => Right(Json(t))
        }
      }

    override def inject(t: Json[T]): String = t.value.asJson.pretty(Printer.noSpaces)

    override implicit def dbType: DbFieldType[String] = stringAdapter.dbType
  }

  implicit def optionalField[T, U](implicit underneathFieldAdapter: FieldAdapter[T, U], nullable: NullableField[U]): FieldAdapter[Option[T], U] =
    new FieldAdapter[Option[T], U] {
      override implicit val dbType: DbFieldType[U] = underneathFieldAdapter.dbType

      override def extract(u: U): Either[ExtractorError, Option[T]] =
        Option(u) match {
          case Some(x) => underneathFieldAdapter.extract(x).map(Some(_))
          case None => Right(None)
        }

      override def inject(t: Option[T]): U = t match {
        case Some(x) => underneathFieldAdapter.inject(x)
        case None => nullable.nullValue
      }
    }

  implicit def arrayField[T, U](implicit fieldAdapter: FieldAdapter[T, U], dbFieldType: DbFieldType.ScalarDbType[U]): FieldAdapter[Seq[T], java.sql.Array] = new FieldAdapter[Seq[T], java.sql.Array] {
    override implicit val dbType: DbFieldType[java.sql.Array] = DbFieldType.ArrayDbType

    override def extract(u: java.sql.Array): Either[ExtractorError, Seq[T]] =
      Try(u.getArray().asInstanceOf[Array[U]])
        .map(array => firstLeftOrRights(array.toSeq.map(fieldAdapter.extract)))
        .getOrElse(Left(ExtractorError("Could not convert array")))

    override def inject(t: Seq[T]): java.sql.Array = ???
  }
}

trait Field[T, U] extends DataSource {
  def fieldAdapter: FieldAdapter[T, U]
}

case class TableField[T, U](name: String, dataSource: Table)(implicit val fieldAdapter: FieldAdapter[T, U]) extends Field[T, U] {
  override def src: String = s"${dataSource.alias}.$name"
  override def alias: String = s"${dataSource.alias}__$name"
}

case class FieldValue[T, U](field: TableField[T, U], value: T) {
  lazy val dbValue: U = field.fieldAdapter.inject(value)
}

object FieldValue {
  implicit def apply[T, U](field: (TableField[T, U], T)): FieldValue[T, U] =
    new FieldValue[T, U](field._1, field._2)
}
