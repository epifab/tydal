package io.epifab.yadl.domain

import java.time.{LocalDate, LocalDateTime}

sealed trait DbType[T] {
  type DBTYPE = T
}

sealed trait PrimitiveDbType[T] extends DbType[T]

case object IntDbType extends PrimitiveDbType[Int]
case object StringDbType extends PrimitiveDbType[String]

sealed trait SeqDbType[T] extends PrimitiveDbType[Seq[T]]
case object IntSeqDbType extends PrimitiveDbType[Seq[Int]] with SeqDbType[Int]
case object StringSeqDbType extends PrimitiveDbType[Seq[String]] with SeqDbType[String]

case class OptionDbType[T](dbType: PrimitiveDbType[T]) extends DbType[Option[T]]

object DbType {
  implicit val int: PrimitiveDbType[Int] = IntDbType
  implicit val string: PrimitiveDbType[String] = StringDbType
  implicit val intSeq: PrimitiveDbType[Seq[Int]] = IntSeqDbType
  implicit val stringSeq: PrimitiveDbType[Seq[String]] = StringSeqDbType
  implicit def option[T](implicit dbType: PrimitiveDbType[T]): DbType[Option[T]] = OptionDbType(dbType)
}
