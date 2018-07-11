package io.epifab.yadl.domain

sealed trait DbType[T]

sealed trait PrimitiveDbType[T] extends DbType[T]

case object IntDbType extends PrimitiveDbType[Int]
case object StringDbType extends PrimitiveDbType[String]
case object DoubleDbType extends PrimitiveDbType[Double]

sealed trait SeqDbType[T] extends PrimitiveDbType[Seq[T]]
case object IntSeqDbType extends PrimitiveDbType[Seq[Int]] with SeqDbType[Int]
case object StringSeqDbType extends PrimitiveDbType[Seq[String]] with SeqDbType[String]
case object DoubleSeqDbType extends PrimitiveDbType[Seq[Double]] with SeqDbType[Double]

final case class OptionDbType[T](dbType: PrimitiveDbType[T]) extends DbType[Option[T]]
