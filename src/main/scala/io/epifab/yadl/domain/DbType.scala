package io.epifab.yadl.domain

sealed trait DbType[T]

sealed abstract class ScalarDbType[T](val name: String) extends DbType[T]

case object IntDbType extends ScalarDbType[Int]("int")
case object StringDbType extends ScalarDbType[String]("varchar")
case object DoubleDbType extends ScalarDbType[Double]("double")
case object DateDbType extends ScalarDbType[String]("date")
case object DateTimeDbType extends ScalarDbType[String]("timestamp")
case object GeometryDbType extends ScalarDbType[String]("geometry")
case object GeographyDbType extends ScalarDbType[String]("geography")
case class EnumDbType(override val name: String) extends ScalarDbType[String](name)

case object IntSeqDbType extends ScalarDbType[Seq[Int]]("int[]")
case object StringSeqDbType extends ScalarDbType[Seq[String]]("varchar[]")
case object DoubleSeqDbType extends ScalarDbType[Seq[Double]]("double[]")
case object DateSeqDbType extends ScalarDbType[Seq[String]]("date[]")
case object DateTimeSeqDbType extends ScalarDbType[Seq[String]]("timestamp[]")
case class EnumSeqDbType(dbType: EnumDbType) extends ScalarDbType[Seq[String]](s"${dbType.name}[]")

case object JsonDbType extends ScalarDbType[String]("json")

case class OptionDbType[T](dbType: ScalarDbType[T]) extends ScalarDbType[Option[T]](dbType.name)
