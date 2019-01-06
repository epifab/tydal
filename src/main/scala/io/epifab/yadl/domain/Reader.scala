package io.epifab.yadl.domain

import shapeless.{::, Generic, HList, HNil, Lazy}

trait Reader[V] {
  type C
  def source: C
  def fields: Seq[Field[_]]
  def extractor: Extractor[V]
}

object Reader {
  type Aux[V, CX] = Reader[V] { type C = CX }

  def apply[C, V](t: C)(implicit readerBuilder: ReaderBuilder.Aux[C, V]): Reader[V] =
    readerBuilder.build(t)
}

trait ReaderBuilder[-C] {
  type Output
  def build(c: C): Reader[Output]
}

class FieldReaderBuilder[T] extends ReaderBuilder[Field[T]] {
  override type Output = T
  override def build(c: Field[T]): Reader[Output] = new Reader[Output] {
    override type C = Field[T]
    override def source: C = c
    override def fields: Seq[Field[_]] = Seq(c)
    override def extractor: Extractor[T] = (row: Row) => row.get(source)
  }
}

class HigherOrderReaderBuilder[V, C] extends ReaderBuilder[Reader[V]] {
  override type Output = V
  override def build(c: Reader[V]): Reader[V] = c
}

object HNilReaderBuilder extends ReaderBuilder[HNil] {
  override type Output = HNil
  override def build(c: HNil): Reader[HNil] = new Reader[Output] {
    override type C = HNil
    override def source: C = c
    override def fields: Seq[Field[_]] = Seq.empty
    override def extractor: Extractor[HNil] = (row: Row) => Right(HNil)
  }
}

class HConsReaderBuilder[H, T <: HList, HC, TC <: HList]
    (implicit
     headBuilder: Lazy[ReaderBuilder.Aux[HC, H]],
     tailBuilder: ReaderBuilder.Aux[TC, T]) extends ReaderBuilder[HC :: TC] {
  override type Output = H :: T
  override def build(c: HC :: TC): Reader[H :: T] = new Reader[Output] {
    private val headReader = headBuilder.value.build(c.head)
    private val tailReader = tailBuilder.build(c.tail)
    override type C = HC :: TC
    override def source: C = c
    override def fields: Seq[Field[_]] = headReader.fields ++ tailReader.fields
    override def extractor: Extractor[H :: T] = (row: Row) => for {
      h <- headReader.extractor.extract(row)
      t <- tailReader.extractor.extract(row)
    } yield ::(h, t)
  }
}

class CaseClassReaderBuilder[CaseClass, GenericRepr, CX](implicit genericBuilder: ReaderBuilder.Aux[CX, GenericRepr], gen: Generic.Aux[CaseClass, GenericRepr]) extends ReaderBuilder[CX] {
  override type Output = CaseClass
  override def build(c: CX): Reader[CaseClass] = new Reader[Output] {
    override type C = CX
    override def source: C = c
    override def fields: Seq[Field[_]] = genericBuilder.build(c).fields
    override def extractor: Extractor[CaseClass] = genericBuilder.build(c).extractor.map(gen.from)
  }
}


object ReaderBuilder {
  type Aux[C, VX] = ReaderBuilder[C] { type Output = VX }

  implicit def fieldReaderBuilder[T]: ReaderBuilder.Aux[Field[T], T] =
    new FieldReaderBuilder[T]

  implicit def hNilReaderBuilder: ReaderBuilder.Aux[HNil, HNil] =
    HNilReaderBuilder

  implicit def hConsReaderBuilder[H, T <: HList, HC, TC <: HList](implicit headBuilder: ReaderBuilder.Aux[HC, H], tailBuilder: ReaderBuilder.Aux[TC, T]): ReaderBuilder.Aux[HC :: TC, H :: T] =
    new HConsReaderBuilder

  implicit def caseClassReaderBuilder[CaseClass, GenericRepr, CX](implicit genericBuilder: ReaderBuilder.Aux[CX, GenericRepr], gen: Generic.Aux[CaseClass, GenericRepr]): ReaderBuilder.Aux[CX, CaseClass] =
    new CaseClassReaderBuilder

  implicit def higherOrderReaderBuilder[C, V]: ReaderBuilder.Aux[Reader[V], V] =
    new HigherOrderReaderBuilder
}
