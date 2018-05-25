package io.epifab.dal.domain

trait DataSource {
  def src: String
  def alias: String
}
