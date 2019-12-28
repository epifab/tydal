package io.epifab.tydal.queries

import java.sql.Connection

import io.epifab.tydal.runtime.{StatementBuilder, WriteStatement, WriteStatementExecutor}
import shapeless.{HList, HNil}

trait WriteQueryBuilder {

  def compile[Placeholders <: HList, InputRepr <: HList](
    implicit
    queryBuilder: QueryBuilder[this.type, Placeholders, HNil],
    statementBuilder: StatementBuilder[Placeholders, InputRepr, HNil],
    statementExecutor: WriteStatementExecutor[Connection, HNil]
  ): WriteStatement[InputRepr, HNil] =
    statementBuilder.build(queryBuilder.build(this)).update

}
