package io.epifab.yadl.domain

import io.circe.{Decoder, Encoder, Printer}
import io.epifab.yadl.domain.FieldAdapter.IntFieldAdapter.U
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

trait FieldAdapter[T] {
  type U
  implicit def dbType: DbType[U]
  def extract(u: U): Either[ExtractorError, T]
  def inject(t: T): U
}

object FieldAdapter {
  type Aux[T, S] = FieldAdapter[T] { type U = S }

  abstract class SimpleFieldAdapter[T](implicit val dbType: DbType[T]) extends FieldAdapter[T] {
    type U = T
    override def extract(u: T): Either[ExtractorError, T] = Right(u)
    override def inject(t: T): T = t
  }

  implicit final case object StringFieldAdapter extends SimpleFieldAdapter[String]

  implicit final case object IntFieldAdapter extends FieldAdapter[Int] {
    type U = java.lang.Integer
    override implicit def dbType: DbType[Integer] = DbType.IntDbType

    override def extract(u: java.lang.Integer): Either[ExtractorError, Int] = Right(u)
    override def inject(t: Int): java.lang.Integer = t
  }

  case class Json[T](value: T)

  implicit def jsonFieldAdapter[T](implicit
                                   stringType: DbType[String],
                                   stringAdapter: FieldAdapter[String],
                                   decoder: Decoder[T],
                                   encoder: Encoder[T]
  ): FieldAdapter[Json[T]] = new FieldAdapter[Json[T]] {
    type U = stringAdapter.U

    import io.circe.parser.decode
    import io.circe.syntax._

    override def extract(u: U): Either[ExtractorError, Json[T]] = stringAdapter.extract(u)
      .flatMap { json =>
        decode(json) match {
          case Left(e) => Left(ExtractorError(e.getMessage))
          case Right(t) => Right(Json(t))
        }
      }

    override def inject(t: Json[T]): U = stringAdapter.inject(t.value.asJson.pretty(Printer.noSpaces))

    override implicit def dbType: DbType[U] = stringAdapter.dbType
  }

  implicit def optionalField[T, S](implicit underneathFieldAdapter: FieldAdapter.Aux[T, S], nullable: NullableField[S]): FieldAdapter.Aux[Option[T], S] =
    new FieldAdapter[Option[T]] {
      type U = S
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

  implicit def seqField[T](implicit underneathFieldAdapter: FieldAdapter[T], dbFieldType: DbType.ScalarDbType[U]): FieldAdapter.Aux[Seq[T], java.sql.Array] = new FieldAdapter[Seq[T]] {
    type U = java.sql.Array
    override implicit val dbType: DbType[java.sql.Array] = DbType.ArrayDbType

    override def extract(u: java.sql.Array): Either[ExtractorError, Seq[T]] =
      Try(u.getArray().asInstanceOf[Array[underneathFieldAdapter.U]])
        .map(array => firstLeftOrRights(array.toSeq.map(underneathFieldAdapter.extract)))
        .getOrElse(Left(ExtractorError("Could not convert array")))

    override def inject(t: Seq[T]): java.sql.Array = ???
  }
}

trait Field[T] extends DataSource {
  def adapter: FieldAdapter[T]
}

trait Value[T] {
  type U
  def adapter: FieldAdapter.Aux[T, U]
  def value: T
  lazy val dbValue: U = adapter.inject(value)
}

case class TableField[T](name: String, dataSource: Table)(implicit val adapter: FieldAdapter[T]) extends Field[T] {
  override def src: String = s"${dataSource.alias}.$name"
  override def alias: String = s"${dataSource.alias}__$name"
}

case class FieldValue[T](field: TableField[T], value: T) extends Value[T] {
  type U = field.adapter.U
  override val adapter: FieldAdapter.Aux[T, U] = field.adapter
}

object FieldValue {
  implicit def apply[T](field: (TableField[T], T)): FieldValue[T] = new FieldValue[T](field._1, field._2)
}
