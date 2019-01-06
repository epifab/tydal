package io.epifab.yadl.domain

import shapeless.{::, Generic, HList, HNil}

sealed trait Reader[V] {
  type C
  def source: C
  def fields: Seq[Field[_]]
  def extractor: Extractor[V]
  def as[V2](implicit gen: Generic.Aux[V2, V]): Reader[V2] =
    new CaseClassReader[V2, V, C](this)
  def ::[HV, HC](head: Reader[HV]): Reader[HV :: HNil] = {
    HConsReader(head.source :: HNil)(head, HNilReader)
  }
}

object Reader {
  type Aux[V, CX] = Reader[V] { type C = CX }
}

case class FieldReader[V](source: Field[V]) extends Reader[V] {
  override type C = Field[V]
  override val extractor: Extractor[V] = (row: Row) => row.get(source)

  override val fields: Seq[Field[_]] = Seq(source)
}

sealed trait HListReader[T <: HList] extends Reader[T] {
  override type C <: HList
  def ::[HV](head: Reader[HV]): HListReader[HV :: T] =
    HConsReader(head.source :: source)(head, this)
}

case object HNilReader extends HListReader[HNil] {
  override type C = HNil
  override val source: C = HNil
  override def extractor: Extractor[HNil] = (row: Row) => Right(HNil)
  override val fields: Seq[Field[_]] = Seq.empty

  def ::[H](h: Field[H]): HConsReader[H, Field[H], HNil, HNil] = {
    HConsReader(h :: HNil)(FieldReader(h), HNilReader)
  }
}

case class HConsReader[H, HC, T <: HList, TC <: HList]
    (source: HC :: TC)
    (implicit
     selectableHead: Reader[H],
     selectableTail: Reader[T])
    extends Reader[H :: T] with HListReader[H :: T] {

  override type C = HC :: TC

  override val extractor: Extractor[H :: T] = (row: Row) => for {
    h <- selectableHead.extractor.extract(row)
    t <- selectableTail.extractor.extract(row)
  } yield shapeless.::(h, t)

  override val fields: Seq[Field[_]] =
    selectableHead.fields ++ selectableTail.fields

  def ::[NH](h: Field[NH]): HConsReader[NH, Field[NH], H :: T, HC :: TC] = {
    HConsReader(h :: source)(FieldReader(h), this)
  }
}

case class CaseClassReader[V, R, CX](baseReader: Reader.Aux[R, CX])(implicit gen: Generic.Aux[V, R]) extends Reader[V] {
  override type C = CX
  override def source: C = baseReader.source

  override val extractor: Extractor[V] = baseReader.extractor.map(gen.from)

  override def fields: Seq[Field[_]] = baseReader.fields
}
