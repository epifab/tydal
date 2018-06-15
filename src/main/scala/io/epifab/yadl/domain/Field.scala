package io.epifab.yadl.domain

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

trait FieldExtractor[T] {
  def extract(v: Any): Either[ExtractorError, T]
}

object FieldExtractor {
  import io.epifab.yadl.utils.EitherSupport._

  implicit val string: FieldExtractor[String] = {
    case s: String => Right(s)
    case _ => Left(ExtractorError("Not a string"))
  }

  implicit val int: FieldExtractor[Int] = {
    case i: Int => Right(i)
    case s: String => Try(s.toInt) match {
      case Success(i) => Right(i)
      case Failure(f) => Left(ExtractorError(f.getMessage))
    }
    case _ => Left(ExtractorError("Not an integer"))
  }

  implicit val long: FieldExtractor[Long] = {
    case l: Long => Right(l)
    case s: String => Try(s.toLong) match {
      case Success(l) => Right(l)
      case Failure(f) => Left(ExtractorError(f.getMessage))
    }
    case _ => Left(ExtractorError("Not a long"))
  }

  implicit val double: FieldExtractor[Double] ={
    case d: Double => Right(d)
    case s: String => Try(s.toDouble) match {
      case Success(d) => Right(d)
      case Failure(f) => Left(ExtractorError(f.getMessage))
    }
    case _ => Left(ExtractorError("Not a double"))
  }

  implicit def sequence[T](implicit extractor: FieldExtractor[T]): FieldExtractor[Seq[T]] = {
    case x: Seq[Any] => firstLeftOrRights(x.map(extractor.extract))
    case _ => Left(ExtractorError("Not a sequence"))
  }

  implicit def optional[T](implicit extractor: FieldExtractor[T]): FieldExtractor[Option[T]] =
    (v: Any) => Option(v) match {
      case Some(x) => extractor.extract(x).map(Some(_))
      case None => Right(None)
    }

  implicit def stringToObject[T](implicit extractor: FieldExtractor[String], ft: String => T): FieldExtractor[T] =
    (v: Any) => extractor.extract(v).map(ft)

}

trait Field[T] extends DataSource {
  def extractor: FieldExtractor[T]
}

case class TableField[T](name: String, dataSource: DataSource)(implicit val extractor: FieldExtractor[T]) extends Field[T] {
  override def src: String = s"${dataSource.alias}.$name"
  override def alias: String = s"${dataSource.alias}__$name"
}

case class FieldValue[T](field: TableField[T], value: T)

object FieldValue {
  implicit def apply[T](field: (TableField[T], T)): FieldValue[T] = new FieldValue(field._1, field._2)
}
