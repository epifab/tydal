package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields._
import io.epifab.yadl.typesafe.runner.{CompiledStatement, StatementBuilder}
import io.epifab.yadl.typesafe.utils.{Appender, TaggedFinder}
import shapeless.{::, Generic, HList, HNil}

trait DataSource[FIELDS] { self: Tag[_] =>
  def fields: FIELDS

  def on[E <: BinaryExpr](clause: this.type => E): Join[this.type, E] =
    new Join(this, clause(this))
}

trait FindContext[HAYSTACK] {
  def apply[TAG <: String with Singleton, X](tag: TAG)(implicit finder: TaggedFinder[TAG, X, HAYSTACK]): X with Tag[TAG]
}

class TableBuilder[NAME <: String, SCHEMA](implicit name: ValueOf[NAME]) {
  def as[ALIAS <: Singleton with String, REPR](alias: ALIAS)(implicit a: ValueOf[ALIAS], generic: Generic.Aux[SCHEMA, REPR], columnsBuilder: ColumnsBuilder[REPR]): Table[NAME, SCHEMA] with Tag[ALIAS] =
    Table(name.value, generic.from(columnsBuilder.build(alias)), alias)
}

class Table[NAME <: String, FIELDS] private(val tableName: String, override val fields: FIELDS) extends DataSource[FIELDS] with FindContext[FIELDS] { self: Tag[_] =>
  override def apply[TAG <: String with Singleton, X](tag: TAG)(implicit finder: TaggedFinder[TAG, X, FIELDS]): X with Tag[TAG] =
    finder.find(fields)

  def `*`: FIELDS = fields
}

object Table {
  def unapply(table: Table[_, _]): Option[String] = Some(table.tableName)

  protected[typesafe] def apply[NAME <: String, FIELDS, ALIAS <: String](tableName: String, fields: FIELDS, tableAlias: String): Table[NAME, FIELDS] with Tag[ALIAS] = new Table[NAME, FIELDS](tableName, fields) with Tag[ALIAS] {
    override def tagValue: String = tableAlias
  }
}

class SubQuery[SUBQUERY_FIELDS <: HList, S <: Select[_, _, _, _]]
    (val select: S, override val fields: SUBQUERY_FIELDS)
    extends DataSource[SUBQUERY_FIELDS] with FindContext[SUBQUERY_FIELDS] { self: Tag[_] =>
  override def apply[TAG <: String with Singleton, X](tag: TAG)(implicit finder: TaggedFinder[TAG, X, SUBQUERY_FIELDS]): X with Tag[TAG] =
    finder.find(fields)
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

class Join[+DS <: DataSource[_], +E <: BinaryExpr](val dataSource: DS, val filter: E)

trait SelectContext[FIELDS <: HList, SOURCES <: HList] extends FindContext[(FIELDS, SOURCES)] {
  def fields: FIELDS
  def sources: SOURCES

  override def apply[TAG <: String with Singleton, X](tag: TAG)(implicit finder: TaggedFinder[TAG, X, (FIELDS, SOURCES)]): X with Tag[TAG] =
    finder.find((fields, sources))

  def apply[TAG1 <: String with Singleton, TAG2 <: String with Singleton, X2, HAYSTACK2]
      (tag1: TAG1, tag2: TAG2)
      (implicit
       finder1: TaggedFinder[TAG1, FindContext[HAYSTACK2], SOURCES],
       finder2: TaggedFinder[TAG2, X2, HAYSTACK2]): X2 AS TAG2 =
    finder1.find(sources).apply[TAG2, X2](tag2)
}

sealed trait Select[FIELDS <: HList, GROUP_BY <: HList, SOURCES <: HList, WHERE <: BinaryExpr] extends SelectContext[FIELDS, SOURCES] { select =>
  def fields: FIELDS
  def groupByFields: GROUP_BY
  def sources: SOURCES
  def filter: WHERE

  class SubQueryBuilder[SUBQUERY_FIELDS <: HList](implicit val refinedFields: SubQueryFields[FIELDS, SUBQUERY_FIELDS]) {
    def as[ALIAS <: String](implicit alias: ValueOf[ALIAS]): SubQuery[SUBQUERY_FIELDS, Select[FIELDS, GROUP_BY, SOURCES, WHERE]] with Tag[ALIAS] =
      new SubQuery(select, refinedFields.build(alias.value, select.fields)) with Tag[ALIAS] {
        override def tagValue: String = alias.value
      }
  }

  def subQuery[SUBQUERY_FIELDS <: HList](implicit subQueryFields: SubQueryFields[FIELDS, SUBQUERY_FIELDS]) =
    new SubQueryBuilder[SUBQUERY_FIELDS]

  def query[PLACEHOLDERS <: HList](implicit queryBuilder: QueryBuilder[this.type, PLACEHOLDERS, FIELDS]): Query[PLACEHOLDERS, FIELDS] =
    queryBuilder.build(this)

  def compile[PLACEHOLDERS <: HList, INPUT <: HList]
    (implicit
     queryBuilder: QueryBuilder[this.type, PLACEHOLDERS, FIELDS],
     statementBuilder: StatementBuilder[PLACEHOLDERS, INPUT, FIELDS]): CompiledStatement[INPUT, FIELDS] =
    statementBuilder.build(queryBuilder.build(this))

  type Statement[F[+_, +_], Connection, Input, Output] = (Connection, Input) => F[DataError, Seq[Output]]

//  def compile[RS, PLACEHOLDERS <: HList, INPUT <: HList]
//    (implicit
//     queryBuilder: QueryBuilder[this.type, PLACEHOLDERS, FIELDS],
//     statementBuilder: StatementBuilder[PLACEHOLDERS, INPUT, FIELDS],
//     dataExtractor: DataExtractor[RS, FIELDS, FIELDS]): CompiledStatement[INPUT, FIELDS] =
//    statementBuilder.build(queryBuilder.build(this))
}

trait EmptySelect extends Select[HNil, HNil, HNil, AlwaysTrue] {
  override val fields: HNil = HNil
  override val groupByFields: HNil = HNil
  override val sources: HNil = HNil
  override val filter: AlwaysTrue = AlwaysTrue

  def from[T <: DataSource[_] with Tag[_]](source: T)(implicit queryBuilder: QueryBuilder[Select[HNil, HNil, T :: HNil, AlwaysTrue], HNil, HNil]): NonEmptySelect[HNil, HNil, T :: HNil, AlwaysTrue] =
    new NonEmptySelect(fields, groupByFields, source :: HNil, filter)
}

class NonEmptySelect[FIELDS <: HList, GROUP_BY <: HList, SOURCES <: HList, WHERE <: BinaryExpr]
    (override val fields: FIELDS,
     override val groupByFields: GROUP_BY,
     override val sources: SOURCES,
     override val filter: WHERE)
    (implicit queryBuilder: QueryBuilder[Select[FIELDS, GROUP_BY, SOURCES, WHERE], _, FIELDS])
    extends Select[FIELDS, GROUP_BY, SOURCES, WHERE] {

  def take[P, NEW_FIELDS <: HList]
    (f: SelectContext[FIELDS, SOURCES] => P)
    (implicit
     generic: Generic.Aux[P, NEW_FIELDS],
     queryBuilder: QueryBuilder[Select[NEW_FIELDS, GROUP_BY, SOURCES, WHERE], _, NEW_FIELDS]):
    NonEmptySelect[NEW_FIELDS, GROUP_BY, SOURCES, WHERE] =
      new NonEmptySelect(generic.to(f(this)), groupByFields, sources, filter)

  def take1[F <: Field[_]]
    (f: SelectContext[FIELDS, SOURCES] => F)
    (implicit
     queryBuilder: QueryBuilder[Select[F :: HNil, GROUP_BY, SOURCES, WHERE], _, F :: HNil]):
    NonEmptySelect[F :: HNil, GROUP_BY, SOURCES, WHERE] =
      new NonEmptySelect(f(this) :: HNil, groupByFields, sources, filter)

  def groupBy[P, NEW_GROUPBY <: HList]
    (f: SelectContext[FIELDS, SOURCES] => P)
    (implicit
     generic: Generic.Aux[P, NEW_GROUPBY],
     queryBuilder: QueryBuilder[Select[FIELDS, NEW_GROUPBY, SOURCES, WHERE], _, FIELDS]):
    NonEmptySelect[FIELDS, NEW_GROUPBY, SOURCES, WHERE] =
      new NonEmptySelect(fields, generic.to(f(this)), sources, filter)

  def groupBy1[F <: Field[_]]
    (f: SelectContext[FIELDS, SOURCES] => F)
    (implicit
     queryBuilder: QueryBuilder[Select[FIELDS, F :: HNil, SOURCES, WHERE], _, FIELDS]):
    NonEmptySelect[FIELDS, F :: HNil, SOURCES, WHERE] =
      new NonEmptySelect(fields, f(this) :: HNil, sources, filter)

  def join[NEW_SOURCE <: DataSource[_] with Tag[_], JOIN_CLAUSE <: BinaryExpr, SOURCE_RESULTS <: HList]
    (f: SelectContext[FIELDS, SOURCES] => Join[NEW_SOURCE, JOIN_CLAUSE])
    (implicit
     appender: Appender.Aux[SOURCES, Join[NEW_SOURCE, JOIN_CLAUSE], SOURCE_RESULTS],
     queryBuilder: QueryBuilder[Select[FIELDS, GROUP_BY, SOURCE_RESULTS, WHERE], _, FIELDS]):
    NonEmptySelect[FIELDS, GROUP_BY, SOURCE_RESULTS, WHERE] =
      new NonEmptySelect(fields, groupByFields, appender.append(sources, f(this)), filter)

  def where[NEW_WHERE <: BinaryExpr]
    (f: SelectContext[FIELDS, SOURCES] => NEW_WHERE)
    (implicit queryBuilder: QueryBuilder[Select[FIELDS, GROUP_BY, SOURCES, NEW_WHERE], _, FIELDS]): NonEmptySelect[FIELDS, GROUP_BY, SOURCES, NEW_WHERE] =
    new NonEmptySelect(fields, groupByFields, sources, f(this))
}

object Select extends EmptySelect
