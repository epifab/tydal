package io.epifab.tydal.queries

import io.epifab.tydal.runtime.{ReadStatementStep0, StatementBuilder, TagOutput}
import shapeless.HList

trait ReadQueryBuilder[Fields <: HList] {

  def compile[Placeholders <: HList, InputRepr <: HList, OutputRepr <: HList, TaggedOutput <: HList](
    implicit
    queryBuilder: QueryBuilder[this.type, Placeholders, Fields],
    statementBuilder: StatementBuilder[Placeholders, InputRepr, Fields],
    taggedOutput: TagOutput[Fields, OutputRepr, TaggedOutput],
  ): ReadStatementStep0[InputRepr, Fields, OutputRepr, TaggedOutput] =
    statementBuilder.build(queryBuilder.build(this)).select

}
