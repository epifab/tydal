package io.epifab.yadl.domain.typesafe

import io.epifab.yadl.domain.typesafe.DataSource.DataSourceFinder
import io.epifab.yadl.domain.FieldAdapter
import io.epifab.yadl.utils.{Appender, Concat, Finder}
import shapeless.{::, HList, HNil}

object DataSource {
  trait DataSourceFinder[X, U] {
    def find(u: U): X
  }

  object DataSourceFinder {
    implicit def joinedFinder[X <: DataSource[_], T <: HList]: DataSourceFinder[X, Join[X] :: T] =
      (u: Join[X] :: T) => u.head.dataSource

    implicit def headFinder[X, T <: HList]: DataSourceFinder[X, X :: T] =
      (u: X :: T) => u.head

    implicit def tailFinder[X, H, T <: HList](implicit finder: DataSourceFinder[X, T]): DataSourceFinder[X, H :: T] =
      (u: H :: T) => finder.find(u.tail)

    implicit def tuple2FinderA[X, A, B](implicit finder: DataSourceFinder[X, A]): DataSourceFinder[X, (A, B)] =
      (u: (A, B)) => finder.find(u._1)

    implicit def tuple2FinderB[X, A, B](implicit finder: DataSourceFinder[X, B]): DataSourceFinder[X, (A, B)] =
      (u: (A, B)) => finder.find(u._2)

    implicit def tuple3FinderA[X, A, B, C](implicit finder: DataSourceFinder[X, A]): DataSourceFinder[X, (A, B, C)] =
      (u: (A, B, C)) => finder.find(u._1)

    implicit def tuple3FinderB[X, A, B, C](implicit finder: DataSourceFinder[X, B]): DataSourceFinder[X, (A, B, C)] =
      (u: (A, B, C)) => finder.find(u._2)

    implicit def tuple3FinderC[X, A, B, C](implicit finder: DataSourceFinder[X, C]): DataSourceFinder[X, (A, B, C)] =
      (u: (A, B, C)) => finder.find(u._3)
  }
}

trait DataSource[TERMS <: HList] extends Taggable {
  def `*`: TERMS

  def term[X](implicit finder: Finder[X, TERMS]): X =
    finder.find(*)

  def on(clause: this.type => BinaryExpr): Join[this.type] =
    new Join(this, clause(this))
}

abstract class Table[NAME <: String, TERMS <: HList](implicit valueOf: ValueOf[NAME], terms: Terms[TERMS]) extends DataSource[TERMS] {
  def tableName: String = valueOf.value
  def `*`: TERMS = terms.get(this)
}

class Join[+DS <: DataSource[_]](val dataSource: DS, filter: BinaryExpr)

trait SelectContext[PLACEHOLDERS <: HList, SOURCES <: HList] {
  def placeholders: PLACEHOLDERS
  def sources: SOURCES

  def placeholder[X](implicit finder: Finder[X, PLACEHOLDERS]): X =
    finder.find(placeholders)

  def source[X](implicit dataSourceFinder: DataSourceFinder[X, SOURCES]): X =
    dataSourceFinder.find(sources)
}

sealed trait Select[PLACEHOLDERS <: HList, TERMS <: HList, GROUPBY <: HList, SOURCES <: HList]
    extends SelectContext[PLACEHOLDERS, SOURCES] with DataSource[TERMS] {
  def placeholders: PLACEHOLDERS
  def terms: TERMS
  def groupByTerms: GROUPBY
  def sources: SOURCES

  def `*`: TERMS = terms
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

  def withPlaceholder[T, U](implicit fieldAdapter: FieldAdapter[T]): NonEmptySelect[(Placeholder[T] with Alias[U]) :: PLACEHOLDERS, TERMS, GROUPBY, SOURCES] =
    new NonEmptySelect(new Placeholder[T].as[U] :: placeholders, terms, groupByTerms, sources)

  def where(f: SelectContext[PLACEHOLDERS, SOURCES] => BinaryExpr): NonEmptySelect[PLACEHOLDERS, TERMS, GROUPBY, SOURCES] =
    new NonEmptySelect(placeholders, terms, groupByTerms, sources, where and f(this))
}

object Select extends EmptySelect
