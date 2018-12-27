package io.epifab.yadl

import cats.Applicative
import cats.implicits._
import io.epifab.yadl.domain.Filter.Expression.Clause
import io.epifab.yadl.domain.Filter.Expression.Clause.AnyLiteral
import io.epifab.yadl.domain.Filter.{BinaryExpression, Expression, UniaryExpression}
import io.epifab.yadl.domain._

import scala.language.higherKinds

object implicits {
  implicit class ExtendedTypedSelect[F[_]: Applicative, V, C](select: TypedSelect[V, C])(implicit queryRunner: QueryRunner[F]) {
    def fetchOne: F[Either[DALError, Option[V]]] =
      queryRunner.run(select)(select.selectable.extract).map(_.map(_.headOption))
  }

  implicit class ExtendedSelect[F[_]](select: Select)(implicit queryRunner: QueryRunner[F], a: Applicative[F]) {
    def fetchOne[T](implicit extractor: Extractor[T]): F[Either[DALError, Option[T]]] =
      queryRunner.run(select).map(_.map(_.headOption))

    def fetchMany[T](implicit extractor: Extractor[T]): F[Either[DALError, Seq[T]]] =
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
      BinaryExpression(clause, AnyLiteral(v), Expression.Op.Equal)
  }

  implicit class ExtendedValueClause[T](value: Value[T]) extends ExtendedClause[T] {
    override val clause: Clause[T] = Expression.Clause.Literal(value)
  }

  implicit class ExtendedColumnClause[T](column: io.epifab.yadl.domain.Column[T]) extends ExtendedClause[T] {
    override val clause = Expression.Clause.Column(column)

    def asc: Sort = AscSort(column)
    def desc: Sort = DescSort(column)
  }

  implicit class ExtendedStringColumn(column: io.epifab.yadl.domain.Column[String]) {
    def like(ec: ExtendedClause[String]): Expression =
      BinaryExpression(Expression.Clause.Column(column), ec.clause, Expression.Op.Like)
  }

  implicit class ExtendedOptionStringColumn(column: io.epifab.yadl.domain.Column[Option[String]]) {
    def like(ec: ExtendedClause[String]): Expression =
      BinaryExpression(Expression.Clause.Column(column), ec.clause, Expression.Op.Like)
  }

  implicit class ExtendedOptionColumn[T](column: io.epifab.yadl.domain.Column[Option[T]]) {
    def isDefined: Expression =
      UniaryExpression(Expression.Clause.Column(column), Expression.Op.IsDefined)

    def isNotDefined: Expression =
      UniaryExpression(Expression.Clause.Column(column), Expression.Op.IsNotDefined)
  }

  trait ExtendedSeqClause[T] {
    def clause: Clause[Seq[T]]

    def contains(ec: ExtendedClause[Seq[T]]): Expression =
      BinaryExpression(clause, ec.clause, Expression.Op.Contains)

    def overlaps(ec: ExtendedClause[Seq[T]]): Expression =
      BinaryExpression(clause, ec.clause, Expression.Op.Overlaps)
  }

  implicit class ExtendedSeqColumnClause[T](column: io.epifab.yadl.domain.Column[Seq[T]]) extends ExtendedSeqClause[T] {
    override def clause: Clause[Seq[T]] = Clause.Column(column)
  }

  implicit class ExtendedSeqValueClause[T](value: Value[Seq[T]]) extends ExtendedSeqClause[T] {
    override def clause: Clause[Seq[T]] = Clause.Literal(value)
  }
}
