package tydal.queries

import tydal.schema.Field

sealed trait SortOrder

case class SortBy[+F <: Field[_]](field: F, sortOrder: SortOrder)

object Ascending {
  case object AscendingOrder extends SortOrder

  def apply[F <: Field[_]](field: F): SortBy[F] =
    SortBy(field, AscendingOrder)
}

object Descending {
  case object DescendingOrder extends SortOrder

  def apply[F <: Field[_]](field: F): SortBy[F] =
    SortBy(field, DescendingOrder)
}
