package io.epifab.yadl.domain

sealed trait DbType[T]

sealed trait PrimitiveDbType[T] extends DbType[T]

case object IntDbType extends PrimitiveDbType[Int]
case object StringDbType extends PrimitiveDbType[String]
case object DoubleDbType extends PrimitiveDbType[Double]

case object IntSeqDbType extends PrimitiveDbType[Seq[Int]]
case object StringSeqDbType extends PrimitiveDbType[Seq[String]]
case object DoubleSeqDbType extends PrimitiveDbType[Seq[Double]]

case object JsonDbType extends PrimitiveDbType[String]
case object DateDbType extends PrimitiveDbType[String]
case object DateTimeDbType extends PrimitiveDbType[String]
final case class EnumDbType(name: String) extends PrimitiveDbType[String]

final case class OptionDbType[T](dbType: PrimitiveDbType[T]) extends DbType[Option[T]]
