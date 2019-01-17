package io.epifab.yadl.domain

import shapeless.{::, Generic, HList, HNil, Lazy}

trait Terms[Output] {
  type Container
  def container: Container
  def toSeq: Seq[Term[_]]
  def extractor: Extractor[Output]
}

object Terms {
  type Aux[Output, ContainerX] = Terms[Output] { type Container = ContainerX }

  def apply[Container, Output](c: Container)(implicit builder: TermsBuilder.Aux[Container, Output]): Terms[Output] =
    builder.build(c)
}

trait TermsBuilder[-Container] {
  type Output
  def build(c: Container): Terms[Output]
}

class TermBuilder[T] extends TermsBuilder[Term[T]] {
  override type Output = T
  override def build(c: Term[T]): Terms[Output] = new Terms[Output] {
    override type Container = Term[T]
    override def container: Container = c
    override def toSeq: Seq[Term[_]] = Seq(c)
    override def extractor: Extractor[T] = (row: Row) => row.get(c)
  }
}

class HigherOrderTermsBuilder[OutputX, Container] extends TermsBuilder[Terms[OutputX]] {
  override type Output = OutputX
  override def build(c: Terms[Output]): Terms[Output] = c
}

object HNilTermsBuilder extends TermsBuilder[HNil] {
  override type Output = HNil
  override def build(c: HNil): Terms[HNil] = new Terms[Output] {
    override type Container = HNil
    override def container: Container = c
    override def toSeq: Seq[Term[_]] = Seq.empty
    override def extractor: Extractor[HNil] = (row: Row) => Right(HNil)
  }
}

class HConsTermsBuilder[H, T <: HList, HC, TC <: HList]
    (implicit
     headBuilder: Lazy[TermsBuilder.Aux[HC, H]],
     tailBuilder: TermsBuilder.Aux[TC, T]) extends TermsBuilder[HC :: TC] {
  override type Output = H :: T
  override def build(c: HC :: TC): Terms[H :: T] = new Terms[Output] {
    private val headReader: Terms[H] = headBuilder.value.build(c.head)
    private val tailReader: Terms[T] = tailBuilder.build(c.tail)

    override type Container = HC :: TC
    override def container: Container = c
    override def toSeq: Seq[Term[_]] = headReader.toSeq ++ tailReader.toSeq

    override def extractor: Extractor[H :: T] = (row: Row) => for {
      h <- headReader.extractor.extract(row)
      t <- tailReader.extractor.extract(row)
    } yield ::(h, t)
  }
}

class CaseClassTermsBuilder[CaseClass, GenericRepr, ContainerX]
    (implicit
     genericBuilder: TermsBuilder.Aux[ContainerX, GenericRepr],
     gen: Generic.Aux[CaseClass, GenericRepr]) extends TermsBuilder[ContainerX] {
  override type Output = CaseClass
  override def build(c: ContainerX): Terms[CaseClass] = new Terms[Output] {
    override type Container = ContainerX
    override def container: Container = c
    override def toSeq: Seq[Term[_]] = genericBuilder.build(c).toSeq
    override def extractor: Extractor[CaseClass] = genericBuilder.build(c).extractor.map(gen.from)
  }
}


object TermsBuilder {
  type Aux[Container, OutputX] = TermsBuilder[Container] { type Output = OutputX }

  implicit def termBuilder[T]: TermsBuilder.Aux[Term[T], T] =
    new TermBuilder[T]

  implicit def hNilTermsBuilder: TermsBuilder.Aux[HNil, HNil] =
    HNilTermsBuilder

  implicit def hConsTermsBuilder[H, T <: HList, HC, TC <: HList]
      (implicit
       headBuilder: TermsBuilder.Aux[HC, H],
       tailBuilder: TermsBuilder.Aux[TC, T]): TermsBuilder.Aux[HC :: TC, H :: T] =
    new HConsTermsBuilder

  implicit def caseClassTermsBuilder[CaseClass, GenericRepr, Container]
      (implicit
       builder: TermsBuilder.Aux[Container, GenericRepr],
       gen: Generic.Aux[CaseClass, GenericRepr]): TermsBuilder.Aux[Container, CaseClass] =
    new CaseClassTermsBuilder

  implicit def higherOrderTermsBuilder[Output]: TermsBuilder.Aux[Terms[Output], Output] =
    new HigherOrderTermsBuilder
}
