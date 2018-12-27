package io.epifab.yadl.domain

import shapeless.{::, Generic, HList, HNil}

sealed trait Selectable[V, C] {
  def source: C
  def columns: Seq[Column[_]]
  def aggregations: Seq[AggregateColumn[_, _]]
  def extract(row: Row): Either[ExtractorError, V]
  def as[V2](implicit gen: Generic.Aux[V2, V]): Selectable[V2, C] =
    new SelectableProduct[V2, V, C](this)
}

case class SelectableColumn[V](override val source: Column[V]) extends Selectable[V, Column[V]] {
  override def extract(row: Row): Either[ExtractorError, V] = row.get(source)
  override val columns: Seq[Column[_]] = Seq(source)
  override val aggregations: Seq[AggregateColumn[_, _]] =
    Seq(source).collect { case a: AggregateColumn[_, _] => a }
}

case object SNil extends Selectable[Unit, Unit] {
  case object SelectableHNil extends Selectable[HNil, HNil] {
    override val source: HNil = HNil
    override def extract(row: Row): Either[ExtractorError, HNil] = Right(HNil)
    override val columns: Seq[Column[_]] = Seq.empty
    override val aggregations: Seq[AggregateColumn[_, _]] = Seq.empty
  }

  override val source: Unit = Unit
  override def extract(row: Row): Either[ExtractorError, Unit] = Right(Unit)
  override val columns: Seq[Column[_]] = Seq.empty
  override val aggregations: Seq[AggregateColumn[_, _]] = Seq.empty

  def +:[H](h: Column[H]): SelectableHList[H, Column[H], HNil, HNil] = {
    val head: Selectable[H, Column[H]] = SelectableColumn(h)
    SelectableHList(head.source :: HNil)(head, SelectableHNil)
  }

  def +:[V, C](head: Selectable[V, C]): SelectableHList[V, C, HNil, HNil] = {
    SelectableHList(head.source :: HNil)(head, SelectableHNil)
  }
}

case class SelectableHList[H, HC, T <: HList, TC <: HList]
    (override val source: HC :: TC)
    (implicit
     selectableHead: Selectable[H, HC],
     selectableTail: Selectable[T, TC])
    extends Selectable[H :: T, HC :: TC] {

  override def extract(row: Row): Either[ExtractorError, H :: T] = {
    for {
      h <- selectableHead.extract(row)
      t <- selectableTail.extract(row)
    } yield ::(h, t)
  }

  override val columns: Seq[Column[_]] =
    selectableHead.columns ++ selectableTail.columns

  override val aggregations: Seq[AggregateColumn[_, _]] =
    selectableHead.aggregations ++ selectableTail.aggregations

  def +:[NH](h: Column[NH]): SelectableHList[NH, Column[NH], H :: T, HC :: TC] = {
    val head: Selectable[NH, Column[NH]] = SelectableColumn(h)
    SelectableHList(head.source :: source)(head, this)
  }

  def +:[V2, C2](head: Selectable[V2, C2]): SelectableHList[V2, C2, H :: T, HC :: TC] = {
    SelectableHList(head.source :: source)(head, this)
  }
}

class SelectableProduct[V, R, C](base: Selectable[R, C])(implicit gen: Generic.Aux[V, R]) extends Selectable[V, C] {
  override def source: C = base.source

  override def extract(row: Row): Either[ExtractorError, V] =
    base.extract(row).map(gen.from)

  override def columns: Seq[Column[_]] = base.columns
  override def aggregations: Seq[AggregateColumn[_, _]] = base.aggregations
}
