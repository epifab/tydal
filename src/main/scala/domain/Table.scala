package domain

case class Table(src: String, alias: String) extends DataSource

object Table {
  def apply(name: String): Table = new Table(name, name)
}
