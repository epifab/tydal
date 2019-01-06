package io.epifab.yadl.domain

import shapeless.{::, Generic, HList, HNil, Lazy}

trait Writer[V] extends Reader[V] {
  def fields: Seq[Column[_]]
  def values(v: V): Seq[ColumnValue[_]]
}

object Writer {
  type Aux[V, CX] = Writer[V] { type C = CX }

  def apply[C, V](t: C)(implicit readerBuilder: WriterBuilder.Aux[C, V]): Writer[V] =
    readerBuilder.build(t)
}

trait WriterBuilder[-C] {
  type Output
  def build(c: C): Writer[Output]
}

class ColumnWriterBuilder[T] extends WriterBuilder[Column[T]] {
  override type Output = T
  override def build(c: Column[T]): Writer[Output] = new Writer[Output] {
    override type C = Column[T]
    override def source: C = c
    override def fields: Seq[Column[_]] = Seq(c)
    override def extractor: Extractor[T] = (row: Row) => row.get(source)
    override def values(v: T): Seq[ColumnValue[_]] = Seq(ColumnValue(c -> v))
  }
}

object HNilWriterBuilder extends WriterBuilder[HNil] {
  override type Output = HNil
  override def build(c: HNil): Writer[HNil] = new Writer[Output] {
    override type C = HNil
    override def source: C = c
    override def fields: Seq[Column[_]] = Seq.empty
    override def extractor: Extractor[HNil] = (row: Row) => Right(HNil)
    override def values(v: HNil): Seq[ColumnValue[_]] = Seq.empty
  }
}

class HConsWriterBuilder[H, T <: HList, HC, TC <: HList]
    (implicit
     headBuilder: Lazy[WriterBuilder.Aux[HC, H]],
     tailBuilder: WriterBuilder.Aux[TC, T]) extends WriterBuilder[HC :: TC] {
  override type Output = H :: T
  override def build(c: HC :: TC): Writer[H :: T] = new Writer[Output] {
    private val headWriter = headBuilder.value.build(c.head)
    private val tailWriter = tailBuilder.build(c.tail)

    override type C = HC :: TC
    override def source: C = c
    override def fields: Seq[Column[_]] = headWriter.fields ++ tailWriter.fields
    override def extractor: Extractor[H :: T] = (row: Row) => for {
      h <- headWriter.extractor.extract(row)
      t <- tailWriter.extractor.extract(row)
    } yield ::(h, t)
    override def values(v: H :: T): Seq[ColumnValue[_]] = headWriter.values(v.head) ++ tailWriter.values(v.tail)
  }
}

class CaseClassWriterBuilder[CaseClass, GenericRepr, CX](implicit genericBuilder: WriterBuilder.Aux[CX, GenericRepr], gen: Generic.Aux[CaseClass, GenericRepr]) extends WriterBuilder[CX] {
  override type Output = CaseClass
  override def build(c: CX): Writer[CaseClass] = new Writer[Output] {
    override type C = CX
    override def source: C = c
    override def fields: Seq[Column[_]] = genericBuilder.build(c).fields
    override def extractor: Extractor[CaseClass] = genericBuilder.build(c).extractor.map(gen.from)
    override def values(v: CaseClass): Seq[ColumnValue[_]] = genericBuilder.build(c).values(gen.to(v))
  }
}


object WriterBuilder {
  type Aux[C, VX] = WriterBuilder[C] { type Output = VX }

  implicit def fieldWriterBuilder[T]: WriterBuilder.Aux[Column[T], T] =
    new ColumnWriterBuilder[T]

  implicit def hNilWriterBuilder: WriterBuilder.Aux[HNil, HNil] =
    HNilWriterBuilder

  implicit def hConsWriterBuilder[H, T <: HList, HC, TC <: HList](implicit headBuilder: WriterBuilder.Aux[HC, H], tailBuilder: WriterBuilder.Aux[TC, T]): WriterBuilder.Aux[HC :: TC, H :: T] =
    new HConsWriterBuilder

  implicit def caseClassWriterBuilder[CaseClass, GenericRepr, CX](implicit genericBuilder: WriterBuilder.Aux[CX, GenericRepr], gen: Generic.Aux[CaseClass, GenericRepr]): WriterBuilder.Aux[CX, CaseClass] =
    new CaseClassWriterBuilder
}
