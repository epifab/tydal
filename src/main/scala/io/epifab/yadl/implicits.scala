package io.epifab.yadl

import cats.Applicative
import cats.implicits._
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

  trait ExtendedClause[T, U] {
    def clause: Expression.Clause[T]

    def ===(ec: ExtendedClause[T, U]): Expression =
      BinaryExpression(clause, ec.clause, Expression.Op.Equal)

    def !==(ec: ExtendedClause[T, U]): Expression =
      BinaryExpression(clause, ec.clause, Expression.Op.NotEqual)

    def > (ec: ExtendedClause[T, U]): Expression =
      BinaryExpression(clause, ec.clause, Expression.Op.GT)

    def < (ec: ExtendedClause[T, U]): Expression =
      BinaryExpression(clause, ec.clause, Expression.Op.LT)

    def >= (ec: ExtendedClause[T, U]): Expression =
      BinaryExpression(clause, ec.clause, Expression.Op.GTE)

    def <= (ec: ExtendedClause[T, U]): Expression =
      BinaryExpression(clause, ec.clause, Expression.Op.LTE)

    def in[V](ec: ExtendedClause[Seq[T], V]): Expression =
      BinaryExpression(clause, ec.clause, Expression.Op.In)
  }

  implicit class ExtendedValue[T, U](value: T)(implicit fieldType: FieldAdapter[T, U]) extends ExtendedClause[T, U] {
    override val clause = Expression.Clause.Literal(value)
  }

  implicit class ExtendedField[T, U](field: io.epifab.yadl.domain.Field[T, U]) extends ExtendedClause[T, U] {
    override val clause = Expression.Clause.Field(field)

    def asc: Sort = AscSort(field)
    def desc: Sort = DescSort(field)
  }

  implicit class ExtendedStringField(field: io.epifab.yadl.domain.Field[String, java.lang.String]) {
    def like(ec: ExtendedClause[String, String]): Expression =
      BinaryExpression(Expression.Clause.Field(field), ec.clause, Expression.Op.Like)
  }

  implicit class ExtendedOptionStringField(field: io.epifab.yadl.domain.Field[Option[String], java.lang.String]) {
    def like(ec: ExtendedClause[String, String]): Expression =
      BinaryExpression(Expression.Clause.Field(field), ec.clause, Expression.Op.Like)
  }

  implicit class ExtendedOptionField[T, U](field: io.epifab.yadl.domain.Field[Option[T], U]) {
    def isDefined: Expression =
      UniaryExpression(Expression.Clause.Field(field), Expression.Op.IsDefined)

    def isNotDefined: Expression =
      UniaryExpression(Expression.Clause.Field(field), Expression.Op.IsNotDefined)
  }
}
