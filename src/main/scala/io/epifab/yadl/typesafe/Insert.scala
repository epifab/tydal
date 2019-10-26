package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields.{ColumnsBuilder, FieldValues}
import shapeless.{Generic, HList}
import shapeless.ops.hlist.Tupler

class InsertInto[NAME <: String, SCHEMA, RAW_VALUES <: HList, VALUES]
(val table: Table[NAME, SCHEMA])(implicit generic: Generic.Aux[VALUES, RAW_VALUES]) {
  def set(values: VALUES): Insert[NAME, SCHEMA, VALUES] =
    new Insert(table, values)
}

class Insert[NAME <: String, SCHEMA, VALUES](val table: Table[NAME, SCHEMA], val values: VALUES) {

}

object Insert {
  def into[NAME <: String, SCHEMA, FIELDS <: HList, RAW_VALUES <: HList, VALUES]
      (tableBuilder: TableBuilder[NAME, SCHEMA])
      (implicit
       name: ValueOf[NAME],
       genericSchema: Generic.Aux[SCHEMA, FIELDS],
       columnsBuilder: ColumnsBuilder[FIELDS],
       values: FieldValues[FIELDS, RAW_VALUES],
       tupler: Tupler.Aux[RAW_VALUES, VALUES],
       genericValues: Generic.Aux[VALUES, RAW_VALUES]
      ): InsertInto[NAME, SCHEMA, RAW_VALUES, VALUES] =
    new InsertInto(tableBuilder as name.value)
}
