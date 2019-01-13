package io.epifab.yadl

import io.epifab.yadl.domain.Filter.Expression.Clause
import io.epifab.yadl.domain.Filter.Expression.Clause.AnyTerm
import io.epifab.yadl.domain.Filter.{BinaryExpression, Expression, UniaryExpression}
import io.epifab.yadl.domain._
import cats.Applicative
import cats.implicits._

import scala.language.higherKinds

object implicits {
  implicit class ExtendedSelect[F[_], V, C](select: Select[V])(implicit queryRunner: QueryRunner[F], a: Applicative[F]) {
    def fetchOne: F[Either[DALError, Option[V]]] =
      fetchMany.map(_.map(_.headOption))

    def fetchMany: F[Either[DALError, Seq[V]]] =
      queryRunner.run(select)
  }

  implicit class ExtendedStatementWithSideEffects[F[_]](statement: Statement with SideEffect)(implicit queryRunner: QueryRunner[F]) {
    def execute(): F[Either[DALError, Int]] =
      queryRunner.run(statement)
  }

  trait ExtendedClause[T] {
    def clause: Expression.Clause[T]

    def ===(ec: ExtendedClause[T]): Expression =
      BinaryExpression(clause, ec.clause, Expression.Op.Equal)

    def !==(ec: ExtendedClause[T]): Expression =
      BinaryExpression(clause, ec.clause, Expression.Op.NotEqual)

    def > (ec: ExtendedClause[T]): Expression =
      BinaryExpression(clause, ec.clause, Expression.Op.GT)

    def < (ec: ExtendedClause[T]): Expression =
      BinaryExpression(clause, ec.clause, Expression.Op.LT)

    def >= (ec: ExtendedClause[T]): Expression =
      BinaryExpression(clause, ec.clause, Expression.Op.GTE)

    def <= (ec: ExtendedClause[T]): Expression =
      BinaryExpression(clause, ec.clause, Expression.Op.LTE)

    def in(v: Value[Seq[T]]): Expression =
      BinaryExpression(clause, AnyTerm(v), Expression.Op.Equal)
  }

  implicit class ExtendedClauseTerm[T](term: io.epifab.yadl.domain.Term[T]) extends ExtendedClause[T] {
    override val clause = Expression.Clause.Term(term)

    def asc: Sort = AscSort(term)
    def desc: Sort = DescSort(term)
  }

  implicit class ExtendedStringTerm(term: io.epifab.yadl.domain.Term[String]) {
    def like(ec: ExtendedClause[String]): Expression =
      BinaryExpression(Expression.Clause.Term(term), ec.clause, Expression.Op.Like)
  }

  implicit class ExtendedOptionStringTerm(term: io.epifab.yadl.domain.Term[Option[String]]) {
    def like(ec: ExtendedClause[String]): Expression =
      BinaryExpression(Expression.Clause.Term(term), ec.clause, Expression.Op.Like)
  }

  implicit class ExtendedOptionTerm[T](term: io.epifab.yadl.domain.Term[Option[T]]) {
    def isDefined: Expression =
      UniaryExpression(Expression.Clause.Term(term), Expression.Op.IsDefined)

    def isNotDefined: Expression =
      UniaryExpression(Expression.Clause.Term(term), Expression.Op.IsNotDefined)
  }

  trait ExtendedSeqClause[T] {
    def clause: Clause[Seq[T]]

    def contains(ec: ExtendedClause[Seq[T]]): Expression =
      BinaryExpression(clause, ec.clause, Expression.Op.Contains)

    def overlaps(ec: ExtendedClause[Seq[T]]): Expression =
      BinaryExpression(clause, ec.clause, Expression.Op.Overlaps)
  }

  implicit class ExtendedClauseTermSeq[T](term: io.epifab.yadl.domain.Term[Seq[T]]) extends ExtendedSeqClause[T] {
    override def clause: Clause[Seq[T]] = Clause.Term(term)
  }
}
