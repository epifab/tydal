package tydal.queries

import tydal.runtime.{ReadStatementStep0, StatementBuilder, TagOutput}
import tydal.utils.HSet
import shapeless.HList

trait ReadQueryBuilder[Fields <: HList] {

  def compile[Placeholders <: HList, InputRepr <: HList, InputSet <: HList, OutputRepr <: HList, TaggedOutput <: HList](
    implicit
    queryBuilder: QueryBuilder[this.type, Placeholders, Fields],
    statementBuilder: StatementBuilder[Placeholders, InputRepr, Fields],
    taggedOutput: TagOutput[Fields, OutputRepr, TaggedOutput],
    hSet: HSet[InputRepr, InputSet]
  ): ReadStatementStep0[InputSet, Fields, OutputRepr, TaggedOutput] =
    statementBuilder.build(queryBuilder.build(this)).contramap(hSet.toList).select

}
