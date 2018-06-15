package io.epifab.yadl.domain

trait DataSource {
  def src: String
  def alias: String
}
