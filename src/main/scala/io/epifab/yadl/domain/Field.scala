package io.epifab.yadl.domain

import io.circe.{Decoder, Encoder, Printer}
import io.epifab.yadl.utils.EitherSupport._

import scala.language.implicitConversions
import scala.util.Try

sealed trait DbType[T]

object DbType {
  sealed trait ScalarDbType[T] extends DbType[T]
  implicit final case object StringDbType extends ScalarDbType[java.lang.String]
  implicit final case object IntDbType extends ScalarDbType[java.lang.Integer]
  implicit final case object ArrayDbType extends DbType[java.sql.Array]
}

trait NullableField[T] {
  def nullValue: T
}

object NullableField {
  implicit val nullableString: NullableField[java.lang.String] = new NullableField[java.lang.String] {
    override def nullValue: java.lang.String = null
  }

  implicit val nullableInt: NullableField[java.lang.Integer] = new NullableField[java.lang.Integer] {
    override def nullValue: java.lang.Integer = null
  }

  implicit val nullableArray: NullableField[java.sql.Array] = new NullableField[java.sql.Array] {
    override def nullValue: java.sql.Array = null
  }
}

trait FieldAdapter[T, U] {
  implicit def dbType: DbType[U]
  def extract(u: U): Either[ExtractorError, T]
  def inject(t: T): U
}

object FieldAdapter {
  abstract class SimpleFieldAdapter[T](implicit val dbType: DbType[T]) extends FieldAdapter[T, T] {
    override def extract(u: T): Either[ExtractorError, T] = Right(u)
    override def inject(t: T): T = t
  }

  implicit final case object StringFieldAdapter extends SimpleFieldAdapter[String]

  implicit final case object IntFieldAdapter extends FieldAdapter[Int, java.lang.Integer] {
    override implicit def dbType: DbType[Integer] = DbType.IntDbType

    override def extract(u: java.lang.Integer): Either[ExtractorError, Int] = Right(u)
    override def inject(t: Int): java.lang.Integer = t
  }

  case class Json[T](value: T)

  implicit def jsonFieldAdapter[T](implicit
                                   dbType: DbType[String],
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

    override implicit def dbType: DbType[String] = stringAdapter.dbType
  }

  implicit def optionalField[T, U](implicit underneathFieldAdapter: FieldAdapter[T, U], nullable: NullableField[U]): FieldAdapter[Option[T], U] =
    new FieldAdapter[Option[T], U] {
      override implicit val dbType: DbType[U] = underneathFieldAdapter.dbType

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

  implicit def arrayField[T, U](implicit underneathFieldAdapter: FieldAdapter[T, U], dbFieldType: DbType.ScalarDbType[U]): FieldAdapter[Seq[T], java.sql.Array] = new FieldAdapter[Seq[T], java.sql.Array] {
    override implicit val dbType: DbType[java.sql.Array] = DbType.ArrayDbType

    override def extract(u: java.sql.Array): Either[ExtractorError, Seq[T]] =
      Try(u.getArray().asInstanceOf[Array[U]])
        .map(array => firstLeftOrRights(array.toSeq.map(underneathFieldAdapter.extract)))
        .getOrElse(Left(ExtractorError("Could not convert array")))

    override def inject(t: Seq[T]): java.sql.Array = ???
  }
}

trait Field[T, U] extends DataSource {
  def adapter: FieldAdapter[T, U]
}

trait Value[T, U] {
  def adapter: FieldAdapter[T, U]
  def value: T
  lazy val dbValue: U = adapter.inject(value)
}

case class TableField[T, U](name: String, dataSource: Table)(implicit val adapter: FieldAdapter[T, U]) extends Field[T, U] {
  override def src: String = s"${dataSource.alias}.$name"
  override def alias: String = s"${dataSource.alias}__$name"
}

case class FieldValue[T, U](field: TableField[T, U], value: T) extends Value[T, U] {
  override val adapter: FieldAdapter[T, U] = field.adapter
}

object FieldValue {
  implicit def apply[T, U](field: (TableField[T, U], T)): FieldValue[T, U] =
    new FieldValue[T, U](field._1, field._2)
}
