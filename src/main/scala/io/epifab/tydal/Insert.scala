package io.epifab.tydal

import io.epifab.tydal.fields.ColumnsBuilder
import io.epifab.tydal.runner.{QueryBuilder, StatementBuilder, WriteStatement}
import shapeless.ops.hlist.Tupler
import shapeless.{Generic, HList, HNil}

class Insert[NAME <: Tag, SCHEMA](val table: Table[NAME, SCHEMA]) {
  def compile[PLACEHOLDERS <: HList, RAW_INPUT <: HList, INPUT]
      (implicit
       queryBuilder: QueryBuilder[this.type, PLACEHOLDERS, HNil],
       statementBuilder: StatementBuilder[PLACEHOLDERS, RAW_INPUT, INPUT, HNil],
       tupler: Tupler.Aux[RAW_INPUT, INPUT]
      ): WriteStatement[INPUT, HNil] =
    statementBuilder.build(queryBuilder.build(this)).update
}

object Insert {
  def into[NAME <: Tag, SCHEMA, FIELDS <: HList]
      (tableBuilder: TableBuilder[NAME, SCHEMA])
      (implicit
       name: ValueOf[NAME],
       genericSchema: Generic.Aux[SCHEMA, FIELDS],
       columnsBuilder: ColumnsBuilder[FIELDS]
      ): Insert[NAME, SCHEMA] =
    new Insert(tableBuilder as name.value)
}
