package io.epifab.yadl.typesafe.runner

import io.epifab.yadl.typesafe.fields.Value

trait RunnableStatement {
  def sql: String
  def input: Seq[Value[_]]
}
