package io.epifab.tydal.queries

import java.sql.Connection

import io.epifab.tydal.runtime.{StatementBuilder, WriteStatement, WriteStatementExecutor}
import io.epifab.tydal.utils.HSet
import shapeless.{HList, HNil}

trait WriteQueryBuilder {

  def compile[Placeholders <: HList, InputRepr <: HList, InputSet <: HList](
    implicit
    queryBuilder: QueryBuilder[this.type, Placeholders, HNil],
    statementBuilder: StatementBuilder[Placeholders, InputRepr, HNil],
    statementExecutor: WriteStatementExecutor[Connection, HNil],
    hSet: HSet[InputRepr, InputSet]
  ): WriteStatement[InputSet, HNil] =
    statementBuilder.build(queryBuilder.build(this)).contramap(hSet.toList).update

}
