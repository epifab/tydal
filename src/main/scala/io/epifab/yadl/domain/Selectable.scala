package io.epifab.yadl.domain

import shapeless.{::, Generic, HList, HNil}

sealed trait Selectable[V] {
  type C
  def source: C
  def fields: Seq[Field[_]]
  def extractor: Extractor[V]
  def as[V2](implicit gen: Generic.Aux[V2, V]): Selectable[V2] =
    new SelectableProduct[V2, V, C](this)
  def ++:[HV, HC](head: Selectable[HV]): Selectable[HV :: HNil] = {
    SCons(head.source :: HNil)(head, SNil)
  }
}

object Selectable {
  type Aux[V, C2] = Selectable[V] { type C = C2}
}

case class SelectableColumn[V](source: Field[V]) extends Selectable[V] {
  override type C = Field[V]
  override val extractor: Extractor[V] = (row: Row) => row.get(source)

  override val fields: Seq[Field[_]] = Seq(source)
}

sealed trait SHList[T <: HList] extends Selectable[T] {
  override type C <: HList
  def ++:[HV](head: Selectable[HV]): SHList[HV :: T] =
    SCons(head.source :: source)(head, this)
}

case object SNil extends SHList[HNil] {
  override type C = HNil
  override val source: C = HNil
  override def extractor: Extractor[HNil] = (row: Row) => Right(HNil)
  override val fields: Seq[Field[_]] = Seq.empty

  def +:[H](h: Field[H]): SCons[H, Field[H], HNil, HNil] = {
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

  override val extractor: Extractor[H :: T] = (row: Row) => for {
    h <- selectableHead.extractor.extract(row)
    t <- selectableTail.extractor.extract(row)
  } yield ::(h, t)

  override val fields: Seq[Field[_]] =
    selectableHead.fields ++ selectableTail.fields

  def +:[NH](h: Field[NH]): SCons[NH, Field[NH], H :: T, HC :: TC] = {
    SCons(h :: source)(SelectableColumn(h), this)
  }
}

class SelectableProduct[V, R, CX](base: Selectable[R]{type C = CX})(implicit gen: Generic.Aux[V, R]) extends Selectable[V] {
  override type C = CX
  override def source: C = base.source

  override val extractor: Extractor[V] = base.extractor.map(gen.from)

  override def fields: Seq[Field[_]] = base.fields
}
