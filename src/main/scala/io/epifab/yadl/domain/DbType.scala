package io.epifab.yadl.domain

sealed trait DbType[T]

sealed trait PrimitiveDbType[T] extends DbType[T]

case object IntDbType extends PrimitiveDbType[Int]
case object StringDbType extends PrimitiveDbType[String]
case class OptionDbType[T](dbType: DbType[T]) extends DbType[Option[T]]
case class SeqDbType[T](dbType: DbType[T]) extends DbType[Seq[T]]

object DbType {
  implicit val int: PrimitiveDbType[Int] = IntDbType
  implicit val string: PrimitiveDbType[String] = StringDbType
  implicit def option[T](implicit dbType: PrimitiveDbType[T]): DbType[Option[T]] =
    OptionDbType(dbType)
  implicit def seq[T](implicit dbType: PrimitiveDbType[T]): DbType[Seq[T]] =
    SeqDbType(dbType)
}
