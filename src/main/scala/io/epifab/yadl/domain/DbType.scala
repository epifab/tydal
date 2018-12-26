package io.epifab.yadl.domain

sealed trait DbType[T]

case object IntDbType extends DbType[Int]
case object StringDbType extends DbType[String]
case object DoubleDbType extends DbType[Double]
case object DateDbType extends DbType[String]
case object DateTimeDbType extends DbType[String]
case object JsonDbType extends DbType[String]
case class EnumDbType(name: String) extends DbType[String]

case object IntSeqDbType extends DbType[Seq[Int]]
case object StringSeqDbType extends DbType[Seq[String]]
case object DoubleSeqDbType extends DbType[Seq[Double]]
case object DateSeqDbType extends DbType[Seq[String]]
case object DateTimeSeqDbType extends DbType[Seq[String]]
case class EnumSeqDbType(dbType: EnumDbType) extends DbType[Seq[String]]

case class OptionDbType[T](dbType: DbType[T]) extends DbType[Option[T]]
