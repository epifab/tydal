package io.epifab.yadl

import io.epifab.yadl.domain.Filter.{BinaryExpression, Expression, UniaryExpression}
import io.epifab.yadl.domain.{AscSort, DescSort, Sort}

object implicits {
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

    def in(ec: ExtendedClause[Iterable[T]]): Expression =
      BinaryExpression(clause, ec.clause, Expression.Op.In)
  }

  implicit class ExtendedValue[T](value: T) extends ExtendedClause[T] {
    override val clause = Expression.Clause.Literal(value)
  }

  implicit class ExtendedField[T](field: io.epifab.yadl.domain.Field[T]) extends ExtendedClause[T] {
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
