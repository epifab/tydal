package domain

sealed trait Filter {
  def and(filter: Filter): Filter = Filter.And(this, filter)
  def or(filter: Filter): Filter = Filter.Or(this, filter)
}

object Filter {
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
    override val clause = Expression.Clause.Value(value)
  }

  implicit class ExtendedField[T](field: domain.Field[T]) extends ExtendedClause[T] {
    override val clause = Expression.Clause.Field(field)
  }

  final case object Empty extends Filter {
    override def and(filter: Filter): Filter = filter
    override def or(filter: Filter): Filter = filter
  }
  final case class Or(filter1: Filter, filter2: Filter) extends Filter
  final case class And(filter1: Filter, filter2: Filter) extends Filter

  final case class Expression(left: Expression.Clause[_], right: Expression.Clause[_], op: Expression.Op) extends Filter

  object Expression {
    sealed trait Clause[T]
    object Clause {
      case class Field[T](field: domain.Field[T]) extends Clause[T]
      case class Value[T](value: T) extends Clause[T]
    }

    sealed trait Op
    object Op {
      final case object Equal extends Op
      final case object NotEqual extends Op
      final case object GT extends Op
      final case object LT extends Op
      final case object GTE extends Op
      final case object LTE extends Op
      final case object Like extends Op
      final case object In extends Op
    }
  }
}
