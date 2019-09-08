package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields._
import io.epifab.yadl.typesafe.utils.{Appender, FindByTag, FindByNestedTag, TaggedFinder}
import shapeless.{::, HList, HNil}

trait DataSource[FIELDS] {
  def fields: FIELDS

  def on(clause: this.type => BinaryExpr): Join[this.type] =
    new Join(this, clause(this))
}

trait FindContext[HAYSTACK] {
  def apply[TAG <: String](implicit tag: ValueOf[TAG]): FindByTag[TAG, HAYSTACK]
}

class TableBuilder[NAME <: String, FIELDS <: HList] {
  def as[ALIAS <: String](implicit name: ValueOf[NAME], alias: ValueOf[ALIAS], columnsBuilder: ColumnsBuilder[FIELDS]): Table[NAME, FIELDS] with Tag[ALIAS] =
    Table(name.value, columnsBuilder.build(alias.value), alias.value)
}

class Table[NAME <: String, FIELDS <: HList] private(tableName: String, override val fields: FIELDS) extends DataSource[FIELDS] with FindContext[FIELDS] { self: Tag[_] =>
  def apply[TAG <: String](implicit tag: ValueOf[TAG]): FindByTag[TAG, FIELDS] =
    new FindByTag(fields)
}

object Table {
  protected[typesafe] def apply[NAME <: String, FIELDS <: HList, ALIAS <: String](tableName: String, fields: FIELDS, tableAlias: String): Table[NAME, FIELDS] with Tag[ALIAS] = new Table[NAME, FIELDS](tableName, fields) with Tag[ALIAS] {
    override def tagValue: String = tableAlias
  }
}

class Join[+DS <: DataSource[_]](val dataSource: DS, filter: BinaryExpr)

trait SelectContext[PLACEHOLDERS <: HList, FIELDS <: HList, SOURCES <: HList] extends FindContext[(PLACEHOLDERS, FIELDS, SOURCES)] {
  def placeholders: PLACEHOLDERS
  def fields: FIELDS
  def sources: SOURCES

  override def apply[TAG <: String](implicit tag: ValueOf[TAG]): FindByTag[TAG, (PLACEHOLDERS, FIELDS, SOURCES)] =
    new FindByTag((placeholders, fields, sources))

  def apply[TAG1 <: String, TAG2 <: String](implicit tag1: ValueOf[TAG1], tag2: ValueOf[TAG2]): FindByNestedTag[TAG1, TAG2, SOURCES] =
    new FindByNestedTag(sources)
}

sealed trait Select[PLACEHOLDERS <: HList, FIELDS <: HList, GROUPBY <: HList, SOURCES <: HList]
    extends SelectContext[PLACEHOLDERS, FIELDS, SOURCES] with DataSource[FIELDS] {
  def placeholders: PLACEHOLDERS
  def fields: FIELDS
  def groupByFIELDs: GROUPBY
  def sources: SOURCES
}

trait EmptySelect extends Select[HNil, HNil, HNil, HNil] {
  override def placeholders: HNil = HNil
  override def fields: HNil = HNil
  override def groupByFIELDs: HNil = HNil
  override def sources: HNil = HNil

  def from[T <: DataSource[_] with Tag[_]](source: T): NonEmptySelect[HNil, HNil, HNil, T :: HNil] =
    new NonEmptySelect(HNil, fields, groupByFIELDs, source :: HNil)
}

class NonEmptySelect[PLACEHOLDERS <: HList, FIELDS <: HList, GROUPBY <: HList, SOURCES <: HList]
    (val placeholders: PLACEHOLDERS, val fields: FIELDS, val groupByFIELDs: GROUPBY, val sources: SOURCES, val where: BinaryExpr = BinaryExpr.empty)
    extends Select[PLACEHOLDERS, FIELDS, GROUPBY, SOURCES] {

  def take[NEW_FIELDS <: HList]
    (f: SelectContext[PLACEHOLDERS, FIELDS, SOURCES] => NEW_FIELDS):
    NonEmptySelect[PLACEHOLDERS, NEW_FIELDS, GROUPBY, SOURCES] =
      new NonEmptySelect(placeholders, f(this), groupByFIELDs, sources)

  def groupBy[NEW_GROUPBY <: HList]
    (f: SelectContext[PLACEHOLDERS, FIELDS, SOURCES] => NEW_GROUPBY):
    NonEmptySelect[PLACEHOLDERS, FIELDS, NEW_GROUPBY, SOURCES] =
      new NonEmptySelect(placeholders, fields, f(this), sources)

  def join[NEW_SOURCE <: DataSource[_] with Tag[_], SOURCE_RESULTS <: HList]
    (f: SelectContext[PLACEHOLDERS, FIELDS, SOURCES] => Join[NEW_SOURCE])
    (implicit appender: Appender.Aux[SOURCES, Join[NEW_SOURCE], SOURCE_RESULTS]):
    NonEmptySelect[PLACEHOLDERS, FIELDS, GROUPBY, SOURCE_RESULTS] =
      new NonEmptySelect(placeholders, fields, groupByFIELDs, appender.append(sources, f(this)))

  def withPlaceholder[T, TAG <: String](implicit encoder: FieldEncoder[T], decoder: FieldDecoder[T], alias: ValueOf[TAG]): NonEmptySelect[(Placeholder[T, T] with Tag[TAG]) :: PLACEHOLDERS, FIELDS, GROUPBY, SOURCES] =
    new NonEmptySelect(new Placeholder[T, T].as[TAG] :: placeholders, fields, groupByFIELDs, sources)

  def where(f: SelectContext[PLACEHOLDERS, FIELDS, SOURCES] => BinaryExpr): NonEmptySelect[PLACEHOLDERS, FIELDS, GROUPBY, SOURCES] =
    new NonEmptySelect(placeholders, fields, groupByFIELDs, sources, where and f(this))

  def as[TAG <: String](implicit alias: ValueOf[TAG]): NonEmptySelect[PLACEHOLDERS, FIELDS, GROUPBY, SOURCES] with Tag[TAG] =
    new NonEmptySelect(placeholders, fields, groupByFIELDs, sources, where) with Tag[TAG] {
      override def tagValue: String = alias.value
    }
}

object Select extends EmptySelect
