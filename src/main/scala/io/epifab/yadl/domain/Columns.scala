package io.epifab.yadl.domain

import shapeless.{::, Generic, HList, HNil, Lazy}

trait Columns[Output] extends Terms[Output] {
  def toSeq: Seq[Column[_]]
  def values(v: Output): Seq[ColumnValue[_]]
}

object Columns {
  type Aux[Output, ContainerX] = Columns[Output] { type Container = ContainerX }

  def apply[Container, Output](c: Container)(implicit builder: ColumnsBuilder.Aux[Container, Output]): Columns[Output] =
    builder.build(c)
}

trait ColumnsBuilder[-Container] {
  type Output
  def build(c: Container): Columns[Output]
}

class ColumnBuilder[T] extends ColumnsBuilder[Column[T]] {
  override type Output = T
  override def build(c: Column[T]): Columns[Output] = new Columns[Output] {
    override type Container = Column[T]
    override def container: Container = c
    override def toSeq: Seq[Column[_]] = Seq(c)
    override def extractor: Extractor[T] = (row: Row) => row.get(c)
    override def values(v: T): Seq[ColumnValue[_]] = Seq(ColumnValue(c -> v))
  }
}

object HNilColumnsBuilder extends ColumnsBuilder[HNil] {
  override type Output = HNil
  override def build(c: HNil): Columns[HNil] = new Columns[Output] {
    override type Container = HNil
    override def container: Container = c
    override def toSeq: Seq[Column[_]] = Seq.empty
    override def extractor: Extractor[HNil] = (row: Row) => Right(c)
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

    override type Container = HC :: TC
    override def container: Container = c
    override def toSeq: Seq[Column[_]] = headWriter.toSeq ++ tailWriter.toSeq

    override def extractor: Extractor[H :: T] = (row: Row) => for {
      h <- headWriter.extractor.extract(row)
      t <- tailWriter.extractor.extract(row)
    } yield ::(h, t)

    override def values(v: H :: T): Seq[ColumnValue[_]] =
      headWriter.values(v.head) ++ tailWriter.values(v.tail)
  }
}

class CaseClassColumnsBuilder[CaseClass, GenericRepr, ContainerX]
    (implicit
     genericBuilder: ColumnsBuilder.Aux[ContainerX, GenericRepr],
     gen: Generic.Aux[CaseClass, GenericRepr]) extends ColumnsBuilder[ContainerX] {
  override type Output = CaseClass
  override def build(c: ContainerX): Columns[CaseClass] = new Columns[Output] {
    private val genericColumns: Columns[GenericRepr] = genericBuilder.build(c)

    override type Container = ContainerX
    override def container: Container = c

    override def toSeq: Seq[Column[_]] = genericColumns.toSeq
    override def extractor: Extractor[CaseClass] = genericColumns.extractor.map(gen.from)
    override def values(v: CaseClass): Seq[ColumnValue[_]] = genericColumns.values(gen.to(v))
  }
}


object ColumnsBuilder {
  type Aux[Container, OutputX] = ColumnsBuilder[Container] { type Output = OutputX }

  implicit def columnBuilder[T]: ColumnsBuilder.Aux[Column[T], T] =
    new ColumnBuilder[T]

  implicit def hNilColumnsBuilder: ColumnsBuilder.Aux[HNil, HNil] =
    HNilColumnsBuilder

  implicit def hConsColumnsBuilder[H, T <: HList, HC, TC <: HList]
      (implicit
       headBuilder: ColumnsBuilder.Aux[HC, H],
       tailBuilder: ColumnsBuilder.Aux[TC, T]): ColumnsBuilder.Aux[HC :: TC, H :: T] =
    new HConsColumnsBuilder

  implicit def caseClassColumnsBuilder[CaseClass, GenericRepr, CX]
      (implicit
       builder: ColumnsBuilder.Aux[CX, GenericRepr],
       gen: Generic.Aux[CaseClass, GenericRepr]): ColumnsBuilder.Aux[CX, CaseClass] =
    new CaseClassColumnsBuilder
}
