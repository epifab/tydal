package io.epifab.yadl

import cats.Applicative
import cats.implicits._
import io.epifab.yadl.domain.Filter.Expression.Clause
import io.epifab.yadl.domain.Filter.{BinaryExpression, Expression, UniaryExpression}
import io.epifab.yadl.domain._

import scala.language.higherKinds

object implicits {
  implicit class ExtendedSelect[F[_]](select: Select)(implicit queryRunner: QueryRunner[F], a: Applicative[F]) {
    def fetchOne[T]()(implicit extractor: Extractor[T]): F[Either[DALError, Option[T]]] =
      queryRunner.run(select).map(_.map(_.headOption))

    def fetchMany[T]()(implicit extractor: Extractor[T]): F[Either[DALError, Seq[T]]] =
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

    def in[V](ec: ExtendedClause[Seq[T]]): Expression =
      BinaryExpression(clause, ec.clause, Expression.Op.In)
  }

  implicit class ExtendedValueClause[T](value: Value[T]) extends ExtendedClause[T] {
    override val clause: Clause[T] = Expression.Clause.Literal(value)
  }

  implicit class ExtendedFieldClause[T](field: io.epifab.yadl.domain.Field[T]) extends ExtendedClause[T] {
    override val clause = Expression.Clause.Field(field)

    def asc: Sort = AscSort(field)
    def desc: Sort = DescSort(field)
  }

  implicit class ExtendedStringField(field: io.epifab.yadl.domain.Field[String]) {
    def like(ec: ExtendedClause[String]): Expression =
      BinaryExpression(Expression.Clause.Field(field), ec.clause, Expression.Op.Like)
  }

  implicit class ExtendedOptionStringField(field: io.epifab.yadl.domain.Field[Option[String]]) {
    def like(ec: ExtendedClause[String]): Expression =
      BinaryExpression(Expression.Clause.Field(field), ec.clause, Expression.Op.Like)
  }

  implicit class ExtendedOptionField[T](field: io.epifab.yadl.domain.Field[Option[T]]) {
    def isDefined: Expression =
      UniaryExpression(Expression.Clause.Field(field), Expression.Op.IsDefined)

    def isNotDefined: Expression =
      UniaryExpression(Expression.Clause.Field(field), Expression.Op.IsNotDefined)
  }
}
