package io.epifab.yadl.typesafe.fields

sealed abstract class DbType[T](val name: String)

case object IntDbType extends DbType[Int]("int")
case object StringDbType extends DbType[String]("varchar")
case object DoubleDbType extends DbType[Double]("float")
case object DateDbType extends DbType[String]("date")
case object DateTimeDbType extends DbType[String]("timestamp")
case object GeometryDbType extends DbType[String]("geometry")
case object GeographyDbType extends DbType[String]("geography")
case class EnumDbType(override val name: String) extends DbType[String](name)

case object IntSeqDbType extends DbType[Seq[Int]]("int[]")
case object StringSeqDbType extends DbType[Seq[String]]("varchar[]")
case object DoubleSeqDbType extends DbType[Seq[Double]]("double[]")
case object DateSeqDbType extends DbType[Seq[String]]("date[]")
case object DateTimeSeqDbType extends DbType[Seq[String]]("timestamp[]")
case class EnumSeqDbType(dbType: EnumDbType) extends DbType[Seq[String]](s"${dbType.name}[]")

case object JsonDbType extends DbType[String]("json")

case class OptionDbType[T](dbType: DbType[T]) extends DbType[Option[T]](dbType.name)
