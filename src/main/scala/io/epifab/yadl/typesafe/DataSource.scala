package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields._
import io.epifab.yadl.typesafe.utils.{Appender, FindByTag}
import shapeless.{::, HList, HNil}

trait DataSource[TERMS] {
  def fields: TERMS
  def field[TAG <: String] = new FindByTag[TAG, TERMS](fields)

  def on(clause: this.type => BinaryExpr): Join[this.type] =
    new Join(this, clause(this))
}

abstract class Table[NAME <: String, TERMS <: HList](implicit val tableNameWrapper: ValueOf[NAME], columnsBuilder: ColumnsBuilder[TERMS])
    extends DataSource[TERMS] {
  val tableName: String = tableNameWrapper.value
  def fields: TERMS = columnsBuilder.build(this)
}

class Join[+DS <: DataSource[_]](val dataSource: DS, filter: BinaryExpr)

trait SelectContext[PLACEHOLDERS <: HList, SOURCES <: HList] {
  def placeholders: PLACEHOLDERS
  def sources: SOURCES

  def placeholder[TAG <: String] = new FindByTag[TAG, PLACEHOLDERS](placeholders)
  def source[TAG <: String] = new FindByTag[TAG, SOURCES](sources)
}

sealed trait Select[PLACEHOLDERS <: HList, TERMS <: HList, GROUPBY <: HList, SOURCES <: HList]
    extends SelectContext[PLACEHOLDERS, SOURCES] with DataSource[TERMS] {
  def placeholders: PLACEHOLDERS
  def fields: TERMS
  def groupByTerms: GROUPBY
  def sources: SOURCES
}

trait EmptySelect extends Select[HNil, HNil, HNil, HNil] {
  override def placeholders: HNil = HNil
  override def fields: HNil = HNil
  override def groupByTerms: HNil = HNil
  override def sources: HNil = HNil

  def from[T <: DataSource[_] with Tag[_]](source: T): NonEmptySelect[HNil, HNil, HNil, T :: HNil] =
    new NonEmptySelect(HNil, fields, groupByTerms, source :: HNil)
}

class NonEmptySelect[PLACEHOLDERS <: HList, TERMS <: HList, GROUPBY <: HList, SOURCES <: HList]
    (val placeholders: PLACEHOLDERS, val fields: TERMS, val groupByTerms: GROUPBY, val sources: SOURCES, val where: BinaryExpr = BinaryExpr.empty)
    extends Select[PLACEHOLDERS, TERMS, GROUPBY, SOURCES] {

  def take[NEW_TERMS <: HList]
    (f: SelectContext[PLACEHOLDERS, SOURCES] => NEW_TERMS):
    NonEmptySelect[PLACEHOLDERS, NEW_TERMS, GROUPBY, SOURCES] =
      new NonEmptySelect(placeholders, f(this), groupByTerms, sources)

  def groupBy[NEW_GROUPBY <: HList]
    (f: SelectContext[PLACEHOLDERS, SOURCES] => NEW_GROUPBY):
    NonEmptySelect[PLACEHOLDERS, TERMS, NEW_GROUPBY, SOURCES] =
      new NonEmptySelect(placeholders, fields, f(this), sources)

  def join[NEW_SOURCE <: DataSource[_] with Tag[_], SOURCE_RESULTS <: HList]
    (f: SelectContext[PLACEHOLDERS, SOURCES] => Join[NEW_SOURCE])
    (implicit appender: Appender.Aux[SOURCES, Join[NEW_SOURCE], SOURCE_RESULTS]):
    NonEmptySelect[PLACEHOLDERS, TERMS, GROUPBY, SOURCE_RESULTS] =
      new NonEmptySelect(placeholders, fields, groupByTerms, appender.append(sources, f(this)))

  def withPlaceholder[T, TAG <: String](implicit encoder: FieldEncoder[T], decoder: FieldDecoder[T], alias: ValueOf[TAG]): NonEmptySelect[(Placeholder[T, T] with Tag[TAG]) :: PLACEHOLDERS, TERMS, GROUPBY, SOURCES] =
    new NonEmptySelect(new Placeholder[T, T].as[TAG] :: placeholders, fields, groupByTerms, sources)

  def where(f: SelectContext[PLACEHOLDERS, SOURCES] => BinaryExpr): NonEmptySelect[PLACEHOLDERS, TERMS, GROUPBY, SOURCES] =
    new NonEmptySelect(placeholders, fields, groupByTerms, sources, where and f(this))

  def as[TAG <: String](implicit alias: ValueOf[TAG]): NonEmptySelect[PLACEHOLDERS, TERMS, GROUPBY, SOURCES] with Tag[TAG] =
    new NonEmptySelect(placeholders, fields, groupByTerms, sources, where) with Tag[TAG] {
      override def tagValue: String = alias.value
    }
}

object Select extends EmptySelect
