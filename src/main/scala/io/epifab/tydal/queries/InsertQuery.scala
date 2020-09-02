package io.epifab.tydal.queries

import io.epifab.tydal.schema._
import io.epifab.tydal.utils.{TaggedFind, TaggedReplace}
import io.epifab.tydal.{As, Tagging}
import shapeless.{Generic, HList}

sealed trait OnConflict

object OnConflict {
  sealed trait ThrowException extends OnConflict
  object ThrowException extends ThrowException

  case class DoNothing[ConflictFields <: HList](fields: ConflictFields) extends OnConflict
  case class DoUpdate[ConflictFields <: HList, UpdatePlaceholders <: HList](fields: ConflictFields, updatePlaceholders: UpdatePlaceholders) extends OnConflict
}

class InsertOnConflict[Fields <: HList, Values <: HList, ConflictFields <: HList](table: Table[Fields], values: Values, conflictFields: ConflictFields) {
  def doUpdate[P, UpdateFields <: HList, UpdatePlaceholders <: HList](
    f: Selectable[Fields] => P)(
    implicit
    generic: Generic.Aux[P, UpdateFields],
    placeholders: NamedPlaceholders[UpdateFields, UpdatePlaceholders]
  ): InsertQuery[Fields, Values, OnConflict.DoUpdate[ConflictFields, UpdatePlaceholders]] =
    new InsertQuery(table, values, OnConflict.DoUpdate[ConflictFields, UpdatePlaceholders](conflictFields, placeholders.get))

  val doNothing: InsertQuery[Fields, Values, OnConflict.DoNothing[ConflictFields]] =
    new InsertQuery(table, values, OnConflict.DoNothing[ConflictFields](conflictFields))
}

final class InsertQuery[Fields <: HList, Values <: HList, ConflictPolicy <: OnConflict](private [tydal] val table: Table[Fields], private [tydal] val values: Values, private [tydal] val onConflict: ConflictPolicy) extends WriteQueryBuilder {

  def set[F <: Field[_] with Tagging[_], A <: String with Singleton, T, NewValues <: HList](field: F As A)(
    implicit
    taggedFind: TaggedFind[A, Field[T], Values],
    taggedReplace: TaggedReplace[A, F, Values, NewValues]
  ): InsertQuery[Fields, NewValues, ConflictPolicy] = new InsertQuery(table, taggedReplace(values, field), onConflict)

  def onConflict[P <: Product, ConflictFields <: HList](
    f: Selectable[Fields] => P)(
    implicit
    generic: Generic.Aux[P, ConflictFields]
  ) = new InsertOnConflict[Fields, Values, ConflictFields](table, values, generic.to(f(table)))

}

object Insert {
  def into[TableName <: String with Singleton, Schema, Fields <: HList, Values <: HList](
    tableBuilder: TableBuilder[TableName, Schema])(
    implicit
    name: ValueOf[TableName],
    genericSchema: GenericSchema.Aux[Schema, Fields],
    placeholders: NamedPlaceholders[Fields, Values],
    column: Columns[Fields]
  ): InsertQuery[Fields, Values, OnConflict.ThrowException] =
    new InsertQuery(tableBuilder as name.value, placeholders.get, OnConflict.ThrowException)
}
