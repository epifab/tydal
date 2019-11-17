package io.epifab.tydal

import io.epifab.tydal.runner.{QueryBuilder, StatementBuilder, WriteStatement}
import shapeless.ops.hlist.Tupler
import shapeless.{HList, HNil}

class Insert[NAME <: Tag, FIELDS <: HList](val table: Table[NAME, FIELDS]) {
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
       schemaBuilder: SchemaBuilder[SCHEMA, FIELDS]
      ): Insert[NAME, FIELDS] =
    new Insert(tableBuilder as name.value)
}
