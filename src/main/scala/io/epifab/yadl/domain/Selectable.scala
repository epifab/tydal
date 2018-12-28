package io.epifab.yadl.domain

import shapeless.{::, Generic, HList, HNil}

sealed trait Selectable[V] {
  type C
  def source: C
  def columns: Seq[Column[_]]
  def aggregations: Seq[AggregateColumn[_, _]]
  def extract(row: Row): Either[ExtractorError, V]
  def as[V2](implicit gen: Generic.Aux[V2, V]): Selectable[V2] =
    new SelectableProduct[V2, V, C](this)
  def ++:[HV, HC](head: Selectable[HV]): Selectable[HV :: HNil] = {
    SCons(head.source :: HNil)(head, SNil)
  }
}

object Selectable {
  type Aux[V, C2] = Selectable[V] { type C = C2}
}

case class SelectableColumn[V](source: Column[V]) extends Selectable[V] {
  override type C = Column[V]
  override def extract(row: Row): Either[ExtractorError, V] = row.get(source)

  override val columns: Seq[Column[_]] =
    Seq(source).filter(!_.isInstanceOf[AggregateColumn[_, _]])

  override val aggregations: Seq[AggregateColumn[_, _]] =
    Seq(source).collect { case a: AggregateColumn[_, _] => a }
}

sealed trait SHList[T <: HList] extends Selectable[T] {
  override type C <: HList
  def ++:[HV](head: Selectable[HV]): SHList[HV :: T] =
    SCons(head.source :: source)(head, this)
}

case object SNil extends SHList[HNil] {
  override type C = HNil
  override val source: C = HNil
  override def extract(row: Row): Either[ExtractorError, HNil] = Right(HNil)
  override val columns: Seq[Column[_]] = Seq.empty
  override val aggregations: Seq[AggregateColumn[_, _]] = Seq.empty

  def +:[H](h: Column[H]): SCons[H, Column[H], HNil, HNil] = {
    SCons(h :: HNil)(SelectableColumn(h), SNil)
  }
}

case class SCons[H, HC, T <: HList, TC <: HList]
    (source: HC :: TC)
    (implicit
     selectableHead: Selectable[H],
     selectableTail: Selectable[T])
    extends Selectable[H :: T] with SHList[H :: T] {

  override type C = HC :: TC

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

  def +:[NH](h: Column[NH]): SCons[NH, Column[NH], H :: T, HC :: TC] = {
    SCons(h :: source)(SelectableColumn(h), this)
  }
}

class SelectableProduct[V, R, CX](base: Selectable[R]{type C = CX})(implicit gen: Generic.Aux[V, R]) extends Selectable[V] {
  override type C = CX
  override def source: C = base.source

  override def extract(row: Row): Either[ExtractorError, V] =
    base.extract(row).map(gen.from)

  override def columns: Seq[Column[_]] = base.columns
  override def aggregations: Seq[AggregateColumn[_, _]] = base.aggregations
}
