package io.epifab.yadl.domain

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

trait NullableField[T] {
  def nullValue: T
}

object NullableField {
  implicit val NullableString: NullableField[String] = null
  implicit val NullableInt: NullableField[Int] = null
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

  implicit final case object StringType extends SimpleFieldAdapter[String]
  implicit final case object IntType extends SimpleFieldAdapter[Int]

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

trait Field[T] extends DataSource {
  def fieldAdapter: FieldAdapter[T, _]
}

case class TableField[T](name: String, dataSource: Table)(implicit val fieldAdapter: FieldAdapter[T, _]) extends Field[T] {
  override def src: String = s"${dataSource.alias}.$name"
  override def alias: String = s"${dataSource.alias}__$name"
}

case class FieldValue[T](field: TableField[T], value: T) {
  def dbValue: Any = field.fieldAdapter.inject(value)
}

object FieldValue {
  implicit def apply[T](field: (TableField[T], T)): FieldValue[T] = new FieldValue(field._1, field._2)
}
