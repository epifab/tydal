package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields.ColumnsBuilder
import io.epifab.yadl.typesafe.runner.{UpdateStatement, UpdateStatementBuilder, UpdateStatementExecutor}
import shapeless.ops.hlist.Tupler
import shapeless.{Generic, HList, HNil}

class Insert[NAME <: String, SCHEMA](val table: Table[NAME, SCHEMA]) {
  def compile[PLACEHOLDERS <: HList, RAW_INPUT <: HList, INPUT]
      (implicit
       queryBuilder: QueryBuilder[this.type, PLACEHOLDERS, HNil],
       statementBuilder: UpdateStatementBuilder[PLACEHOLDERS, RAW_INPUT, INPUT],
       tupler: Tupler.Aux[RAW_INPUT, INPUT]
      ): UpdateStatement[RAW_INPUT, INPUT] =
    statementBuilder.build(queryBuilder.build(this))
}

object Insert {
  def into[NAME <: String, SCHEMA, FIELDS <: HList]
      (tableBuilder: TableBuilder[NAME, SCHEMA])
      (implicit
       name: ValueOf[NAME],
       genericSchema: Generic.Aux[SCHEMA, FIELDS],
       columnsBuilder: ColumnsBuilder[FIELDS]
      ): Insert[NAME, SCHEMA] =
    new Insert(tableBuilder as name.value)
}
