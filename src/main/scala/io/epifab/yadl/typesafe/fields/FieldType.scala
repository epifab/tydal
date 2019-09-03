package io.epifab.yadl.typesafe.fields

sealed abstract class FieldType[T](val sqlName: String)

case object TypeInt extends FieldType[Int]("int")
case object TypeString extends FieldType[String]("varchar")
case object TypeDouble extends FieldType[Double]("float")
case object TypeDate extends FieldType[String]("date")
case object TypeDateTime extends FieldType[String]("timestamp")
case object TypeGeometry extends FieldType[String]("geometry")
case object TypeGeography extends FieldType[String]("geography")
case class TypeEnum(override val sqlName: String) extends FieldType[String](sqlName)

case object TypeIntSeq extends FieldType[Seq[Int]]("int[]")
case object TypeStringSeq extends FieldType[Seq[String]]("varchar[]")
case object TypeDoubleSeq extends FieldType[Seq[Double]]("double[]")
case object TypeDateSeq extends FieldType[Seq[String]]("date[]")
case object TypeDateTimeSeq extends FieldType[Seq[String]]("timestamp[]")
case class TypeEnumSeq(dbType: TypeEnum) extends FieldType[Seq[String]](s"${dbType.sqlName}[]")

case object TypeJson extends FieldType[String]("json")

case class TypeOption[T](dbType: FieldType[T]) extends FieldType[Option[T]](dbType.sqlName)
