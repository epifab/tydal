package domain

import scala.util.{Failure, Success, Try}

trait DataSource {
  def src: String
  def alias: String
}

class ExtractorError(msg: String) extends Error(msg)

trait FieldExtractor[T] {
  def extract(v: Any): Either[ExtractorError, T]
}

object FieldExtractor {
  implicit val string: FieldExtractor[String] = {
    case s: String => Right(s)
    case _ => Left(new ExtractorError("Not a string"))
  }

  implicit val int: FieldExtractor[Int] = {
    case i: Int => Right(i)
    case s: String => Try(s.toInt) match {
      case Success(i) => Right(i)
      case Failure(f) => Left(new ExtractorError(f.getMessage))
    }
    case _ => Left(new ExtractorError("Not an integer"))
  }

  implicit val long: FieldExtractor[Long] = {
    case l: Long => Right(l)
    case s: String => Try(s.toLong) match {
      case Success(l) => Right(l)
      case Failure(f) => Left(new ExtractorError(f.getMessage))
    }
    case _ => Left(new ExtractorError("Not a long"))
  }

  implicit val double: FieldExtractor[Double] ={
    case d: Double => Right(d)
    case s: String => Try(s.toDouble) match {
      case Success(d) => Right(d)
      case Failure(f) => Left(new ExtractorError(f.getMessage))
    }
    case _ => Left(new ExtractorError("Not a double"))
  }

  implicit def sequence[T](implicit extractor: FieldExtractor[T]): FieldExtractor[Seq[T]] = {
    case x: Seq[Any] => firstLeftOrRights(x.map(extractor.extract))
    case _ => Left(new ExtractorError("Not a sequence"))
  }

  implicit def optional[T](implicit extractor: FieldExtractor[T]): FieldExtractor[Option[T]] =
    (v: Any) => Option(v) match {
      case Some(x) => extractor.extract(x).map(Some(_))
      case None => Right(None)
    }

  implicit def stringToObject[T](implicit extractor: FieldExtractor[String], ft: (String) => T): FieldExtractor[T] =
    (v: Any) => extractor.extract(v).map(ft)

  private def firstLeftOrRights[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] =
    s.foldLeft[Either[A, Seq[B]]](Right(Seq.empty[B]))(
      (a: Either[A, Seq[B]], b: Either[A, B]) => {
        a.flatMap(results =>
          b match {
            case Left(error) =>
              // New error
              Left(error)
            case Right(result) =>
              Right(results :+ result)
          }
        )
      }
    )
}

class Field[T](override val src: String, override val alias: String)(implicit val extractor: FieldExtractor[T]) extends DataSource
case class Table(src: String, alias: String) extends DataSource

object Field {
  def apply[T](name: String, dataSource: DataSource)(implicit extractor: FieldExtractor[T]): Field[T] =
    new Field[T](s"${dataSource.alias}.$name", s"${dataSource.alias}__$name")
}

object Table {
  def apply(name: String): Table = new Table(name, name)
}
