package io.epifab.yadl.domain

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}

import io.circe.{Decoder, Encoder, Printer}

import scala.util.Try

trait FieldAdapter[T] {
  type DBTYPE
  def dbType: DbType[DBTYPE]
  def toDb(value: T): DBTYPE
  def fromDb(dbValue: DBTYPE): Either[ExtractorError, T]
}

trait PrimitiveFieldAdapter[T] extends FieldAdapter[T] { outer =>
  override def dbType: PrimitiveDbType[DBTYPE]

  def bimap[U](f: T => U, g: U => T): PrimitiveFieldAdapter[U] = {
    bimap[U](
      (dbValue: T) => Try(f(dbValue))
        .toEither
        .left.map(error => ExtractorError(error.getMessage)),
      g
    )
  }

  def bimap[U](f: T => Option[U], g: U => T): PrimitiveFieldAdapter[U] = {
    bimap[U](
      (dbValue: T) => f(dbValue) match {
        case Some(value) => Right(value)
        case None => Left(ExtractorError(s"Could not decode $dbValue"))
      },
      g
    )
  }

  def bimap[U](f: T => Either[ExtractorError, U], g: U => T): PrimitiveFieldAdapter[U] =
    new PrimitiveFieldAdapter[U] {
      override type DBTYPE = outer.DBTYPE
      override def dbType: PrimitiveDbType[outer.DBTYPE] = outer.dbType
      override def toDb(value: U): outer.DBTYPE = outer.toDb(g(value))
      override def fromDb(dbValue: outer.DBTYPE): Either[ExtractorError, U] = for {
        t <- outer.fromDb(dbValue)
        u <- f(t)
      } yield u
    }
}

abstract class SimpleFieldAdapter[T](override val dbType: PrimitiveDbType[T]) extends PrimitiveFieldAdapter[T] {
  type DBTYPE = T
  def toDb(value: T): DBTYPE = value
  def fromDb(dbValue: DBTYPE): Either[ExtractorError, T] = Right(dbValue)
}

case object StringFieldAdapter extends SimpleFieldAdapter[String](StringDbType)

case object IntFieldAdapter extends SimpleFieldAdapter[Int](IntDbType)

case object DoubleFieldAdapter extends SimpleFieldAdapter[Double](DoubleDbType)

case class OptionFieldAdapter[T, U](override val dbType: DbType[Option[U]], baseAdapter: FieldAdapter.Aux[T, U])
  extends FieldAdapter[Option[T]] {
  override type DBTYPE = Option[baseAdapter.DBTYPE]

  override def toDb(value: Option[T]): DBTYPE = value.map(baseAdapter.toDb)

  override def fromDb(dbValue: DBTYPE): Either[ExtractorError, Option[T]] = dbValue match {
    case None => Right(None)
    case Some(u) => baseAdapter.fromDb(u).map(Some(_))
  }
}

case class SeqFieldAdapter[T, U](override val dbType: PrimitiveDbType[Seq[U]], baseAdapter: FieldAdapter.Aux[T, U])
  extends PrimitiveFieldAdapter[Seq[T]] {
  import io.epifab.yadl.utils.EitherSupport._

  override type DBTYPE = Seq[U]

  override def toDb(value: Seq[T]): Seq[baseAdapter.DBTYPE] = value.map(baseAdapter.toDb)

  override def fromDb(dbValue: Seq[baseAdapter.DBTYPE]): Either[ExtractorError, Seq[T]] =
    firstLeftOrRights(dbValue.map(baseAdapter.fromDb))
}

object StringSeqFieldAdapter extends SeqFieldAdapter[String, String](StringSeqDbType, StringFieldAdapter)
object IntSeqFieldAdapter extends SeqFieldAdapter[Int, Int](IntSeqDbType, IntFieldAdapter)
object DoubleSeqFieldAdapter extends SeqFieldAdapter[Double, Double](DoubleSeqDbType, DoubleFieldAdapter)

case class Json[T](value: T)

object FieldAdapter {
  type Aux[T, U] = FieldAdapter[T] { type DBTYPE = U }

  implicit val string: PrimitiveFieldAdapter[String] = StringFieldAdapter
  implicit val int: PrimitiveFieldAdapter[Int] = IntFieldAdapter
  implicit val double: PrimitiveFieldAdapter[Double] = DoubleFieldAdapter
  implicit val stringSeq: PrimitiveFieldAdapter[Seq[String]] = StringSeqFieldAdapter
  implicit val intSeq: PrimitiveFieldAdapter[Seq[Int]] = IntSeqFieldAdapter
  implicit val doubleSeq: PrimitiveFieldAdapter[Seq[Double]] = DoubleSeqFieldAdapter

  implicit def option[T](implicit baseAdapter: PrimitiveFieldAdapter[T]): FieldAdapter[Option[T]] =
    OptionFieldAdapter[T, baseAdapter.DBTYPE](OptionDbType(baseAdapter.dbType), baseAdapter)

  implicit def json[T](implicit encoder: Encoder[T], decoder: Decoder[T]): PrimitiveFieldAdapter[Json[T]] = {
    import io.circe.parser.decode
    import io.circe.syntax._

    string.bimap[Json[T]](
      json => decode(json)
        .right.map(Json(_))
        .left.map(error => ExtractorError(error.getMessage)),
      _.value.asJson.noSpaces
    )
  }

  implicit val date: PrimitiveFieldAdapter[LocalDate] = {
    string.bimap[LocalDate](
      (dbValue: String) => Try(LocalDate.parse(dbValue, DateTimeFormatter.ofPattern("yyyy-MM-dd")))
        .toEither
        .left.map(error => ExtractorError(error.getMessage)),
      _.format(DateTimeFormatter.ISO_DATE)
    )
  }

  implicit val dateTime: PrimitiveFieldAdapter[LocalDateTime] = {
    string.bimap[LocalDateTime](
      (dbValue: String) => Try(LocalDateTime.parse(dbValue, DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.S")))
        .toEither
        .left.map(error => ExtractorError(error.getMessage)),
      _.format(DateTimeFormatter.ISO_DATE_TIME)
    )
  }
}
