package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields._
import io.epifab.yadl.typesafe.utils.{Appender, FindByTag, FindNestedByTag}
import shapeless.{::, HList, HNil, the}

trait DataSource[FIELDS] { self: Tag[_] =>
  def fields: FIELDS

  def on(clause: this.type => BinaryExpr): Join[this.type] =
    new Join(this, clause(this))
}

trait FindContext[HAYSTACK] {
  def apply[TAG <: String](implicit tag: ValueOf[TAG]): FindByTag[TAG, HAYSTACK]
}

class TableBuilder[NAME <: String, FIELDS <: HList](implicit name: ValueOf[NAME], columnsBuilder: ColumnsBuilder[FIELDS]) {
  def as[ALIAS <: String](implicit alias: ValueOf[ALIAS]): Table[NAME, FIELDS] with Tag[ALIAS] =
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

class SubQuery[SUBQUERYFIELDS <: HList, S <: Select[_, _, _]]
    (val select: S, override val fields: SUBQUERYFIELDS)
    extends DataSource[SUBQUERYFIELDS] with FindContext[SUBQUERYFIELDS] { self: Tag[_] =>
  def apply[TAG <: String](implicit tag: ValueOf[TAG]): FindByTag[TAG, SUBQUERYFIELDS] =
    new FindByTag(fields)
}

trait SubQueryFields[-FIELDS, +SUBQUERYFIELDS] {
  def build(srcAlias: String, fields: FIELDS): SUBQUERYFIELDS
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

class Join[+DS <: DataSource[_]](val dataSource: DS, val filter: BinaryExpr)

trait SelectContext[FIELDS <: HList, SOURCES <: HList] extends FindContext[(FIELDS, SOURCES)] {
  def fields: FIELDS
  def sources: SOURCES

  override def apply[TAG <: String](implicit tag: ValueOf[TAG]): FindByTag[TAG, (FIELDS, SOURCES)] =
    new FindByTag((fields, sources))

  def apply[TAG1 <: String, TAG2 <: String](implicit tag1: ValueOf[TAG1], tag2: ValueOf[TAG2]): FindNestedByTag[TAG1, TAG2, SOURCES] =
    new FindNestedByTag(sources)
}

sealed trait Select[FIELDS <: HList, GROUPBY <: HList, SOURCES <: HList] extends SelectContext[FIELDS, SOURCES] { select =>
  def queryBuilder: QueryBuilder[this.type]

  def fields: FIELDS
  def groupByFields: GROUPBY
  def sources: SOURCES
  def filter: BinaryExpr

  class SubQueryBuilder[SUBQUERYFIELDS <: HList](implicit val refinedFields: SubQueryFields[FIELDS, SUBQUERYFIELDS]) {
    def as[ALIAS <: String](implicit alias: ValueOf[ALIAS]): SubQuery[SUBQUERYFIELDS, Select[FIELDS, GROUPBY, SOURCES]] with Tag[ALIAS] =
      new SubQuery(select, refinedFields.build(alias.value, select.fields)) with Tag[ALIAS] {
        override def tagValue: String = alias.value
      }
  }

  def subQuery[REFINED <: HList](implicit refinedFields: SubQueryFields[FIELDS, REFINED]) =
    new SubQueryBuilder[REFINED]

  lazy val query: Query = queryBuilder.build(this)
}

trait EmptySelect extends Select[HNil, HNil, HNil] {
  override val queryBuilder: QueryBuilder[Select[HNil, HNil, HNil]] =
    the[QueryBuilder[Select[HNil, HNil, HNil]]]

  override val fields: HNil = HNil
  override val groupByFields: HNil = HNil
  override val sources: HNil = HNil
  override val filter: BinaryExpr = AlwaysTrue

  def from[T <: DataSource[_] with Tag[_]](source: T)(implicit queryBuilder: QueryBuilder[Select[HNil, HNil, T :: HNil]]): NonEmptySelect[HNil, HNil, T :: HNil] =
    new NonEmptySelect(fields, groupByFields, source :: HNil, filter)
}

class NonEmptySelect[FIELDS <: HList, GROUPBY <: HList, SOURCES <: HList]
    (override val fields: FIELDS,
     override val groupByFields: GROUPBY,
     override val sources: SOURCES,
     override val filter: BinaryExpr)
    (implicit val queryBuilder: QueryBuilder[Select[FIELDS, GROUPBY, SOURCES]])
    extends Select[FIELDS, GROUPBY, SOURCES] {

  def take[NEW_FIELDS <: HList]
    (f: SelectContext[FIELDS, SOURCES] => NEW_FIELDS)
    (implicit queryBuilder: QueryBuilder[Select[NEW_FIELDS, GROUPBY, SOURCES]]):
    NonEmptySelect[NEW_FIELDS, GROUPBY, SOURCES] =
      new NonEmptySelect(f(this), groupByFields, sources, filter)

  def groupBy[NEW_GROUPBY <: HList]
    (f: SelectContext[FIELDS, SOURCES] => NEW_GROUPBY)
    (implicit queryBuilder: QueryBuilder[Select[FIELDS, NEW_GROUPBY, SOURCES]]):
    NonEmptySelect[FIELDS, NEW_GROUPBY, SOURCES] =
      new NonEmptySelect(fields, f(this), sources, filter)

  def join[NEW_SOURCE <: DataSource[_] with Tag[_], SOURCE_RESULTS <: HList]
    (f: SelectContext[FIELDS, SOURCES] => Join[NEW_SOURCE])
    (implicit
     appender: Appender.Aux[SOURCES, Join[NEW_SOURCE], SOURCE_RESULTS],
     queryBuilder: QueryBuilder[Select[FIELDS, GROUPBY, SOURCE_RESULTS]]):
    NonEmptySelect[FIELDS, GROUPBY, SOURCE_RESULTS] =
      new NonEmptySelect(fields, groupByFields, appender.append(sources, f(this)), filter)

  def where(f: SelectContext[FIELDS, SOURCES] => BinaryExpr): NonEmptySelect[FIELDS, GROUPBY, SOURCES] =
    new NonEmptySelect(fields, groupByFields, sources, filter and f(this))
}

object Select extends EmptySelect
