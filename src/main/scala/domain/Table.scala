package domain

case class Table(src: String, alias: String) extends DataSource {
  def field[T](name: String)(implicit fieldExtractor: FieldExtractor[T]): TableField[T] = TableField[T](name, this)
  def relation(name: String) = Table(name, s"${alias}__$name")
}

object Table {
  def apply(name: String): Table = new Table(name, name)
}
