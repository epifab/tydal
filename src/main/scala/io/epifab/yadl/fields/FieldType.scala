package io.epifab.yadl.fields

sealed abstract class FieldType[+T](val sqlName: String) {
  def toSeq: FieldType[Seq[T]] = TypeSeq(this)
  def toOption: FieldType[Option[T]] = TypeOption(this)
}

case object TypeInt extends FieldType[Int]("int")
case object TypeString extends FieldType[String]("varchar")
case object TypeDouble extends FieldType[Double]("float")
case object TypeDate extends FieldType[String]("date")
case object TypeDateTime extends FieldType[String]("timestamp")
case object TypeGeometry extends FieldType[String]("geometry")
case object TypeGeography extends FieldType[String]("geography")
case class TypeEnum(override val sqlName: String) extends FieldType[String](sqlName)
case object TypeJson extends FieldType[String]("json")
case class TypeSeq[T](dbType: FieldType[T]) extends FieldType[Seq[T]](s"${dbType.sqlName}[]")
case class TypeOption[T](dbType: FieldType[T]) extends FieldType[Option[T]](dbType.sqlName)
