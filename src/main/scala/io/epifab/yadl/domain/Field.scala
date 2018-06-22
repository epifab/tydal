package io.epifab.yadl.domain

import io.circe.{Decoder, Encoder, Printer}
import io.epifab.yadl.utils.EitherSupport._

import scala.language.implicitConversions

sealed trait DbType[T]

object DbType {
  sealed trait ScalarDbType[T] extends DbType[T]
  final case object StringDbType extends ScalarDbType[java.lang.String]
  final case object IntDbType extends ScalarDbType[java.lang.Integer]
  final class SeqDbType[T](dbType: ScalarDbType[T]) extends DbType[Seq[T]]

  implicit val string: ScalarDbType[java.lang.String] = StringDbType
  implicit val int: ScalarDbType[java.lang.Integer] = IntDbType
  implicit def seq[T](implicit dbType: ScalarDbType[T]): DbType[Seq[T]] =
    new SeqDbType[T](dbType)
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

  implicit def nullableArray[T]: NullableField[Seq[T]] = new NullableField[Seq[T]] {
    override def nullValue: Seq[T] = null
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

  implicit case object StringFieldAdapter extends SimpleFieldAdapter[String]

  implicit case object IntFieldAdapter extends FieldAdapter[Int] {
    type U = java.lang.Integer
    override implicit def dbType: DbType[Integer] = DbType.IntDbType

    override def extract(u: java.lang.Integer): Either[ExtractorError, Int] = Right(u)
    override def inject(t: Int): java.lang.Integer = t
  }

  case class Json[T](value: T)

  implicit def jsonField[T](implicit
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

  implicit def seqField[T, S](implicit underneathFieldAdapter: FieldAdapter.Aux[T, S], seqDbType: DbType[Seq[S]]): FieldAdapter.Aux[Seq[T], Seq[S]] = new FieldAdapter[Seq[T]] {
    type U = Seq[S]
    override implicit val dbType: DbType[U] = seqDbType

    override def extract(u: U): Either[ExtractorError, Seq[T]] =
      firstLeftOrRights(u.map(underneathFieldAdapter.extract))

    override def inject(t: Seq[T]): U =
      t.map(underneathFieldAdapter.inject)
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

  override def equals(obj: scala.Any): Boolean = obj match {
    case t: Value[T] => t.value == value
    case _ => false
  }
}

object Value {
  implicit def apply[T](v: T)(implicit a: FieldAdapter[T]): Value[T] = new Value[T] {
    type U = a.U
    override def adapter: FieldAdapter.Aux[T, U] = a
    override def value: T = v
  }
}

case class TableField[T](name: String, dataSource: Table)(implicit val adapter: FieldAdapter[T]) extends Field[T] {
  override def src: String = s"${dataSource.alias}.$name"
  override def alias: String = s"${dataSource.alias}__$name"
}

trait FieldValue[T] extends Value[T] {
  def field: TableField[T]
  def value: T
}

object FieldValue {
  implicit def apply[T](fieldValue: (TableField[T], T)): FieldValue[T] = new FieldValue[T] {
    type U = fieldValue._1.adapter.U
    override val adapter: FieldAdapter.Aux[T, U] = fieldValue._1.adapter
    override val field: TableField[T] = fieldValue._1
    override val value: T = fieldValue._2
  }
}
