package domain

trait DataSource {
  def src: String
  def alias: String
}

trait FieldExtractor[T] {
  def extract(v: Any): T
}

object FieldExtractor {
  implicit val string: FieldExtractor[String] =
    (v: Any) => v.asInstanceOf[String]

  implicit val int: FieldExtractor[Int] =
    (v: Any) => v.asInstanceOf[Int]

  implicit val long: FieldExtractor[Long] =
    (v: Any) => v.asInstanceOf[Long]

  implicit val double: FieldExtractor[Double] =
    (v: Any) => v.asInstanceOf[Double]

  implicit def sequence[T](implicit extractor: FieldExtractor[T]): FieldExtractor[Seq[T]] =
    (v: Any) => v.asInstanceOf[Seq[Any]].map(extractor.extract)

  implicit def optional[T](implicit extractor: FieldExtractor[T]): FieldExtractor[Option[T]] =
    (v: Any) => Option(v).map(extractor.extract)

  implicit def stringToObject[T](implicit extractor: FieldExtractor[String], ft: (String) => T): FieldExtractor[T] =
    (v: Any) => ft(extractor.extract(v))
}

class Field[T](override val src: String, override val alias: String)(implicit val extractor: FieldExtractor[T]) extends DataSource
case class Table(src: String, alias: String) extends DataSource

object Field {
  def apply[T](name: String, dataSource: DataSource)(implicit extractor: FieldExtractor[T]): Field[T] =
    new Field[T](s"${dataSource.alias}.$name", s"${dataSource.alias}__$name")
}

object Table {
  def apply(name: String): Table = new Table(name, name)
}
