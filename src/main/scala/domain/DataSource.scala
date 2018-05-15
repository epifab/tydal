package domain

trait DataSource {
  def src: String
  def alias: String
}

case class Field(src: String, alias: String) extends DataSource
case class Table(src: String, alias: String) extends DataSource

object Field {
  def apply(name: String, dataSource: DataSource): Field =
    new Field(s"${dataSource.alias}.$name", s"${dataSource.alias}__$name")
}

object Table {
  def apply(name: String): Table = new Table(name, name)
}
