package io.epifab.yadl.domain

import shapeless.{HList, HNil, ::}

sealed trait DbType[T]

sealed trait ScalarDbType[T] extends DbType[T]
sealed trait CompositeDbType[L <: HList] extends DbType[L]

case object IntDbType extends ScalarDbType[Int]
case object StringDbType extends ScalarDbType[String]
case object DoubleDbType extends ScalarDbType[Double]
case object DateDbType extends ScalarDbType[String]
case object DateTimeDbType extends ScalarDbType[String]
case object PointDbType extends ScalarDbType[String]
case class EnumDbType(name: String) extends ScalarDbType[String]

case object IntSeqDbType extends ScalarDbType[Seq[Int]]
case object StringSeqDbType extends ScalarDbType[Seq[String]]
case object DoubleSeqDbType extends ScalarDbType[Seq[Double]]
case object DateSeqDbType extends ScalarDbType[Seq[String]]
case object DateTimeSeqDbType extends ScalarDbType[Seq[String]]
case class EnumSeqDbType(dbType: EnumDbType) extends ScalarDbType[Seq[String]]

case object JsonDbType extends ScalarDbType[String]

case class OptionDbType[T](dbType: ScalarDbType[T]) extends ScalarDbType[Option[T]]

object CompositeDbType {
  implicit val empty: CompositeDbType[HNil] =
    new CompositeDbType[HNil] {}

  implicit def nonEmpty[H, T <: HList](implicit head: ScalarDbType[H], tail: CompositeDbType[T]): CompositeDbType[H :: T] =
    new CompositeDbType[H :: T] {}
}
