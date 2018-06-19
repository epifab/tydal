package io.epifab.yadl.domain

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

trait FieldAdapter[T] {
  def extract(v: Any): Either[ExtractorError, T]
  def inject(t: T): Any
}

object FieldAdapter {
  import io.epifab.yadl.utils.EitherSupport._

  sealed trait SimpleFieldAdapter[T] extends FieldAdapter[T] {
    override def extract(v: Any): Either[ExtractorError, T] =
      Try(v.asInstanceOf[T]) match {
        case Success(t) => Right(t)
        case Failure(_) => Left(ExtractorError("Invalid field type"))
      }

    override def inject(t: T): Any = t
  }

  implicit case object StringField extends SimpleFieldAdapter[String]
  implicit case object IntField extends SimpleFieldAdapter[Int]
  implicit case object LongField extends SimpleFieldAdapter[Long]
  implicit case object DoubleField extends SimpleFieldAdapter[Double]

  implicit def optionalField[T](implicit underneathFieldAdapter: SimpleFieldAdapter[T]): FieldAdapter[Option[T]] = new FieldAdapter[Option[T]] {
    override def extract(v: Any): Either[ExtractorError, Option[T]] =
      Option(v) match {
        case Some(x) => underneathFieldAdapter.extract(x).map(Some(_))
        case None => Right(None)
      }

    override def inject(t: Option[T]): Any = t match {
      case Some(x) => underneathFieldAdapter.inject(x)
      case None => null
    }
  }

  implicit def sequenceField[T](implicit underneathFieldAdapter: SimpleFieldAdapter[T]): FieldAdapter[Iterable[T]] = new FieldAdapter[Iterable[T]] {
    override def extract(v: Any): Either[ExtractorError, Iterable[T]] = {
      v match {
        case x: Seq[Any] => firstLeftOrRights(x.map(underneathFieldAdapter.extract))
        case _ => Left(ExtractorError("Not a sequence"))
      }
    }

    override def inject(s: Iterable[T]): Any = s.map(underneathFieldAdapter.inject)
  }
}

trait Field[T] extends DataSource {
  def fieldAdapter: FieldAdapter[T]
}

case class TableField[T](name: String, dataSource: Table)(implicit val fieldAdapter: FieldAdapter[T]) extends Field[T] {
  override def src: String = s"${dataSource.alias}.$name"
  override def alias: String = s"${dataSource.alias}__$name"
}

case class FieldValue[T](field: TableField[T], value: T) {
  def dbValue: Any = field.fieldAdapter.inject(value)
}

object FieldValue {
  implicit def apply[T](field: (TableField[T], T)): FieldValue[T] = new FieldValue(field._1, field._2)
}
