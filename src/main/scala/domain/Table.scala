package domain

case class Table(src: String, alias: String) extends DataSource {
  def apply[T](name: String)(implicit extractor: FieldExtractor[T]): Field[T] = TableField[T](name, this)
}

object Table {
  def apply(name: String): Table = new Table(name, name)
}
