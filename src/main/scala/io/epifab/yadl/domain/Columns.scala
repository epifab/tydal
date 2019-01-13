package io.epifab.yadl.domain

import shapeless.{::, Generic, HList, HNil, Lazy}

trait Columns[V] extends Terms[V] {
  def toSeq: Seq[Column[_]]
  def values(v: V): Seq[ColumnValue[_]]
}

object Columns {
  type Aux[V, CX] = Columns[V] { type C = CX }

  def apply[C, V](t: C)(implicit readerBuilder: ColumnsBuilder.Aux[C, V]): Columns[V] =
    readerBuilder.build(t)
}

trait ColumnsBuilder[-C] {
  type Output
  def build(c: C): Columns[Output]
}

class ColumnBuilder[T] extends ColumnsBuilder[Column[T]] {
  override type Output = T
  override def build(c: Column[T]): Columns[Output] = new Columns[Output] {
    override type C = Column[T]
    override def source: C = c
    override def toSeq: Seq[Column[_]] = Seq(c)
    override def extractor: Extractor[T] = (row: Row) => row.get(source)
    override def values(v: T): Seq[ColumnValue[_]] = Seq(ColumnValue(c -> v))
  }
}

object HNilColumnsBuilder$ extends ColumnsBuilder[HNil] {
  override type Output = HNil
  override def build(c: HNil): Columns[HNil] = new Columns[Output] {
    override type C = HNil
    override def source: C = c
    override def toSeq: Seq[Column[_]] = Seq.empty
    override def extractor: Extractor[HNil] = (row: Row) => Right(HNil)
    override def values(v: HNil): Seq[ColumnValue[_]] = Seq.empty
  }
}

class HConsColumnsBuilder[H, T <: HList, HC, TC <: HList]
    (implicit
     headBuilder: Lazy[ColumnsBuilder.Aux[HC, H]],
     tailBuilder: ColumnsBuilder.Aux[TC, T]) extends ColumnsBuilder[HC :: TC] {
  override type Output = H :: T
  override def build(c: HC :: TC): Columns[H :: T] = new Columns[Output] {
    private val headWriter: Columns[H] = headBuilder.value.build(c.head)
    private val tailWriter: Columns[T] = tailBuilder.build(c.tail)

    override type C = HC :: TC
    override def source: C = c
    override def toSeq: Seq[Column[_]] = headWriter.toSeq ++ tailWriter.toSeq
    override def extractor: Extractor[H :: T] = (row: Row) => for {
      h <- headWriter.extractor.extract(row)
      t <- tailWriter.extractor.extract(row)
    } yield ::(h, t)
    override def values(v: H :: T): Seq[ColumnValue[_]] = headWriter.values(v.head) ++ tailWriter.values(v.tail)
  }
}

class CaseClassColumnsBuilder[CaseClass, GenericRepr, CX](implicit genericBuilder: ColumnsBuilder.Aux[CX, GenericRepr], gen: Generic.Aux[CaseClass, GenericRepr]) extends ColumnsBuilder[CX] {
  override type Output = CaseClass
  override def build(c: CX): Columns[CaseClass] = new Columns[Output] {
    override type C = CX
    override def source: C = c
    override def toSeq: Seq[Column[_]] = genericBuilder.build(c).toSeq
    override def extractor: Extractor[CaseClass] = genericBuilder.build(c).extractor.map(gen.from)
    override def values(v: CaseClass): Seq[ColumnValue[_]] = genericBuilder.build(c).values(gen.to(v))
  }
}


object ColumnsBuilder {
  type Aux[C, VX] = ColumnsBuilder[C] { type Output = VX }

  implicit def columnBuilder[T]: ColumnsBuilder.Aux[Column[T], T] =
    new ColumnBuilder[T]

  implicit def hNilColumnsBuilder: ColumnsBuilder.Aux[HNil, HNil] =
    HNilColumnsBuilder$

  implicit def hConsColumnsBuilder[H, T <: HList, HC, TC <: HList](implicit headBuilder: ColumnsBuilder.Aux[HC, H], tailBuilder: ColumnsBuilder.Aux[TC, T]): ColumnsBuilder.Aux[HC :: TC, H :: T] =
    new HConsColumnsBuilder

  implicit def caseClassColumnsBuilder[CaseClass, GenericRepr, CX](implicit genericBuilder: ColumnsBuilder.Aux[CX, GenericRepr], gen: Generic.Aux[CaseClass, GenericRepr]): ColumnsBuilder.Aux[CX, CaseClass] =
    new CaseClassColumnsBuilder
}
