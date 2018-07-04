package io.epifab.yadl.domain

sealed trait DbType[T] {
  type DBTYPE = T
}

sealed trait PrimitiveDbType[T] extends DbType[T]

case object IntDbType extends PrimitiveDbType[Int]
case object StringDbType extends PrimitiveDbType[String]
case class SeqDbType[T](dbType: PrimitiveDbType[T]) extends DbType[Seq[T]]
case class OptionDbType[T](dbType: DbType[T]) extends DbType[Option[T]]

object DbType {
  implicit val int: PrimitiveDbType[Int] = IntDbType
  implicit val string: PrimitiveDbType[String] = StringDbType
  implicit def seq[T](implicit dbType: PrimitiveDbType[T]): DbType[Seq[T]] =
    SeqDbType(dbType)
  implicit def option[T](implicit dbType: DbType[T]): DbType[Option[T]] =
    OptionDbType(dbType)
}
