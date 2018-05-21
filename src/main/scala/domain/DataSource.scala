package domain

trait DataSource {
  def src: String
  def alias: String
}
