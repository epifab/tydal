package io.epifab.dal

import io.epifab.dal.domain.Filter.Expression

object Implicits {
  trait ExtendedClause[T] {
    def clause: Expression.Clause[T]

    def ===(ec: ExtendedClause[T]): Expression =
      Expression(clause, ec.clause, Expression.Op.Equal)

    def !==(ec: ExtendedClause[T]): Expression =
      Expression(clause, ec.clause, Expression.Op.NotEqual)

    def > (ec: ExtendedClause[T]): Expression =
      Expression(clause, ec.clause, Expression.Op.GT)

    def < (ec: ExtendedClause[T]): Expression =
      Expression(clause, ec.clause, Expression.Op.LT)

    def >= (ec: ExtendedClause[T]): Expression =
      Expression(clause, ec.clause, Expression.Op.GTE)

    def <= (ec: ExtendedClause[T]): Expression =
      Expression(clause, ec.clause, Expression.Op.LTE)

    def like(ec: ExtendedClause[T]): Expression =
      Expression(clause, ec.clause, Expression.Op.Like)

    def in(ec: ExtendedClause[Iterable[T]]): Expression =
      Expression(clause, ec.clause, Expression.Op.In)
  }

  implicit class ExtendedValue[T](value: T) extends ExtendedClause[T] {
    override val clause = Expression.Clause.Literal(value)
  }

  implicit class ExtendedField[T](field: io.epifab.dal.domain.Field[T]) extends ExtendedClause[T] {
    override val clause = Expression.Clause.Field(field)
  }
}
