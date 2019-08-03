package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields.{FieldDecoder, FieldEncoder}
import io.epifab.yadl.typesafe.utils.{AliasFinder, Appender, FindByAlias}
import shapeless.{::, HList, HNil}

trait DataSource[TERMS <: HList] extends Taggable {
  def terms: TERMS
  def term[ALIAS] = new FindByAlias[ALIAS, TERMS](terms)

  def on(clause: this.type => BinaryExpr): Join[this.type] =
    new Join(this, clause(this))
}

abstract class Table[NAME <: String, TERMS <: HList](implicit val tableNameWrapper: ValueOf[NAME], termsBuilder: TermsBuilder[TERMS])
    extends DataSource[TERMS] {
  val tableName: String = tableNameWrapper.value
  def terms: TERMS = termsBuilder.build(this)
}

class Join[+DS <: DataSource[_]](val dataSource: DS, filter: BinaryExpr)

trait SelectContext[PLACEHOLDERS <: HList, SOURCES <: HList] {
  implicit def dataSourceFinder[ALIAS, X <: DataSource[_], T <: HList]: AliasFinder[ALIAS, X, Join[X AS ALIAS] :: T] =
    (u: Join[X AS ALIAS] :: T) => u.head.dataSource

  def placeholders: PLACEHOLDERS
  def sources: SOURCES

  def placeholder[A] = new FindByAlias[A, PLACEHOLDERS](placeholders)
  def source[A] = new FindByAlias[A, SOURCES](sources)
}

sealed trait Select[PLACEHOLDERS <: HList, TERMS <: HList, GROUPBY <: HList, SOURCES <: HList]
    extends SelectContext[PLACEHOLDERS, SOURCES] with DataSource[TERMS] {
  def placeholders: PLACEHOLDERS
  def terms: TERMS
  def groupByTerms: GROUPBY
  def sources: SOURCES
}

trait EmptySelect extends Select[HNil, HNil, HNil, HNil] {
  override def placeholders: HNil = HNil
  override def terms: HNil = HNil
  override def groupByTerms: HNil = HNil
  override def sources: HNil = HNil

  def from[T <: DataSource[_] with Alias[_]](source: T): NonEmptySelect[HNil, HNil, HNil, T :: HNil] =
    new NonEmptySelect(HNil, terms, groupByTerms, source :: HNil)
}

class NonEmptySelect[PLACEHOLDERS <: HList, TERMS <: HList, GROUPBY <: HList, SOURCES <: HList]
    (val placeholders: PLACEHOLDERS, val terms: TERMS, val groupByTerms: GROUPBY, val sources: SOURCES, val where: BinaryExpr = BinaryExpr.empty)
    extends Select[PLACEHOLDERS, TERMS, GROUPBY, SOURCES] {

  def take[NEW_TERMS <: HList]
    (f: SelectContext[PLACEHOLDERS, SOURCES] => NEW_TERMS):
    NonEmptySelect[PLACEHOLDERS, NEW_TERMS, GROUPBY, SOURCES] =
      new NonEmptySelect(placeholders, f(this), groupByTerms, sources)

  def groupBy[NEW_GROUPBY <: HList]
    (f: SelectContext[PLACEHOLDERS, SOURCES] => NEW_GROUPBY):
    NonEmptySelect[PLACEHOLDERS, TERMS, NEW_GROUPBY, SOURCES] =
      new NonEmptySelect(placeholders, terms, f(this), sources)

  def join[NEW_SOURCE <: DataSource[_] with Alias[_], SOURCE_RESULTS <: HList]
    (f: SelectContext[PLACEHOLDERS, SOURCES] => Join[NEW_SOURCE])
    (implicit appender: Appender.Aux[SOURCES, Join[NEW_SOURCE], SOURCE_RESULTS]):
    NonEmptySelect[PLACEHOLDERS, TERMS, GROUPBY, SOURCE_RESULTS] =
      new NonEmptySelect(placeholders, terms, groupByTerms, appender.append(sources, f(this)))

  def withPlaceholder[T, U](implicit encoder: FieldEncoder[T], decoder: FieldDecoder[T]): NonEmptySelect[(Placeholder[T, T] with Alias[U]) :: PLACEHOLDERS, TERMS, GROUPBY, SOURCES] =
    new NonEmptySelect(new Placeholder[T, T].as[U] :: placeholders, terms, groupByTerms, sources)

  def where(f: SelectContext[PLACEHOLDERS, SOURCES] => BinaryExpr): NonEmptySelect[PLACEHOLDERS, TERMS, GROUPBY, SOURCES] =
    new NonEmptySelect(placeholders, terms, groupByTerms, sources, where and f(this))
}

object Select extends EmptySelect
