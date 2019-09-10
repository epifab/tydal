package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields._
import io.epifab.yadl.typesafe.utils.{Appender, FindByTag, FindByNestedTag, TaggedFinder}
import shapeless.{::, HList, HNil}

trait DataSource[FIELDS] { self: Tag[_] =>
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

class Table[NAME <: String, FIELDS <: HList] private(val tableName: String, override val fields: FIELDS) extends DataSource[FIELDS] with FindContext[FIELDS] { self: Tag[_] =>
  def apply[TAG <: String](implicit tag: ValueOf[TAG]): FindByTag[TAG, FIELDS] =
    new FindByTag(fields)
}

object Table {
  def unapply(table: Table[_, _]): Option[String] = Some(table.tableName)

  protected[typesafe] def apply[NAME <: String, FIELDS <: HList, ALIAS <: String](tableName: String, fields: FIELDS, tableAlias: String): Table[NAME, FIELDS] with Tag[ALIAS] = new Table[NAME, FIELDS](tableName, fields) with Tag[ALIAS] {
    override def tagValue: String = tableAlias
  }
}

class SubQuery[PLACEHOLDERS <: HList, FIELDS <: HList, SUBQUERYFIELDS <: HList, GROUPBY <: HList, SOURCES <: HList]
    (val select: Select[PLACEHOLDERS, FIELDS, GROUPBY, SOURCES])
    (implicit subQueryFields: SubQueryFields[FIELDS, SUBQUERYFIELDS]) extends DataSource[SUBQUERYFIELDS] with FindContext[SUBQUERYFIELDS] { self: Tag[_] =>
  override def fields: SUBQUERYFIELDS = subQueryFields.build(tagValue, select.fields)

  def apply[TAG <: String](implicit tag: ValueOf[TAG]): FindByTag[TAG, SUBQUERYFIELDS] =
    new FindByTag(fields)
}

trait SubQueryFields[-FIELDS, +SUBQUERY_FIELDS] {
  def build(srcAlias: String, fields: FIELDS): SUBQUERY_FIELDS
}

object SubQueryFields {
  implicit def singleField[T, ALIAS <: String]: SubQueryFields[Field[T] with Tag[ALIAS], Column[T] with Tag[ALIAS]] =
    (srcAlias: String, field: Field[T] with Tag[ALIAS]) => new Column(field.tagValue, srcAlias)(field.decoder) with Tag[ALIAS] {
      override def tagValue: String = field.tagValue
    }

  implicit def hNil: SubQueryFields[HNil, HNil] =
    (_: String, _: HNil) => HNil

  implicit def hCons[H, RH, T <: HList, RT <: HList](implicit headField: SubQueryFields[H, RH], tailFields: SubQueryFields[T, RT]): SubQueryFields[H :: T, RH :: RT] =
    (srcAlias: String, list: H :: T) => headField.build(srcAlias, list.head) :: tailFields.build(srcAlias, list.tail)
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

sealed trait Select[PLACEHOLDERS <: HList, FIELDS <: HList, GROUPBY <: HList, SOURCES <: HList] extends SelectContext[PLACEHOLDERS, FIELDS, SOURCES] { select =>
  def placeholders: PLACEHOLDERS
  def fields: FIELDS
  def groupByFields: GROUPBY
  def sources: SOURCES

  class SubQueryBuilder[REFINED_FIELDS <: HList](implicit val refinedFields: SubQueryFields[FIELDS, REFINED_FIELDS]) {
    def as[ALIAS <: String](implicit alias: ValueOf[ALIAS]): SubQuery[PLACEHOLDERS, FIELDS, REFINED_FIELDS, GROUPBY, SOURCES] with Tag[ALIAS] =
      new SubQuery(select) with Tag[ALIAS] {
        override def tagValue: String = alias.value
      }
  }

  def subQuery[REFINED <: HList](implicit refinedFields: SubQueryFields[FIELDS, REFINED]) =
    new SubQueryBuilder[REFINED]
}

trait EmptySelect extends Select[HNil, HNil, HNil, HNil] {
  override def placeholders: HNil = HNil
  override def fields: HNil = HNil
  override def groupByFields: HNil = HNil
  override def sources: HNil = HNil

  def from[T <: DataSource[_] with Tag[_]](source: T): NonEmptySelect[HNil, HNil, HNil, T :: HNil] =
    new NonEmptySelect(HNil, fields, groupByFields, source :: HNil)
}

class NonEmptySelect[PLACEHOLDERS <: HList, FIELDS <: HList, GROUPBY <: HList, SOURCES <: HList]
    (val placeholders: PLACEHOLDERS, val fields: FIELDS, val groupByFields: GROUPBY, val sources: SOURCES, val where: BinaryExpr = BinaryExpr.empty)
    extends Select[PLACEHOLDERS, FIELDS, GROUPBY, SOURCES] {

  def take[NEW_FIELDS <: HList]
    (f: SelectContext[PLACEHOLDERS, FIELDS, SOURCES] => NEW_FIELDS):
    NonEmptySelect[PLACEHOLDERS, NEW_FIELDS, GROUPBY, SOURCES] =
      new NonEmptySelect(placeholders, f(this), groupByFields, sources)

  def groupBy[NEW_GROUPBY <: HList]
    (f: SelectContext[PLACEHOLDERS, FIELDS, SOURCES] => NEW_GROUPBY):
    NonEmptySelect[PLACEHOLDERS, FIELDS, NEW_GROUPBY, SOURCES] =
      new NonEmptySelect(placeholders, fields, f(this), sources)

  def join[NEW_SOURCE <: DataSource[_] with Tag[_], SOURCE_RESULTS <: HList]
    (f: SelectContext[PLACEHOLDERS, FIELDS, SOURCES] => Join[NEW_SOURCE])
    (implicit appender: Appender.Aux[SOURCES, Join[NEW_SOURCE], SOURCE_RESULTS]):
    NonEmptySelect[PLACEHOLDERS, FIELDS, GROUPBY, SOURCE_RESULTS] =
      new NonEmptySelect(placeholders, fields, groupByFields, appender.append(sources, f(this)))

  def withPlaceholder[T, TAG <: String](implicit encoder: FieldEncoder[T], decoder: FieldDecoder[T], name: ValueOf[TAG]): NonEmptySelect[(Placeholder[T, T] with Tag[TAG]) :: PLACEHOLDERS, FIELDS, GROUPBY, SOURCES] =
    new NonEmptySelect(new Placeholder[T, T](name.value).as[TAG] :: placeholders, fields, groupByFields, sources)

  def where(f: SelectContext[PLACEHOLDERS, FIELDS, SOURCES] => BinaryExpr): NonEmptySelect[PLACEHOLDERS, FIELDS, GROUPBY, SOURCES] =
    new NonEmptySelect(placeholders, fields, groupByFields, sources, where and f(this))
}

object Select extends EmptySelect
