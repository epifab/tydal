package domain

sealed trait Filter

object Filter {
  final case class Or(filter1: Filter, filter2: Filter) extends Filter
  final case class And(filter1: Filter, filter2: Filter) extends Filter

  final case class Expression(left: Expression.Clause, right: Expression.Clause, op: Expression.Op) extends Filter

  object Expression {
    sealed trait Clause
    object Clause {
      case class Field(field: domain.Field) extends Clause
      case class Value[T](value: T) extends Clause
    }

    sealed trait Op
    object Op {
      final case object Equal extends Op
      final case object NotEqual extends Op
    }
  }
}
