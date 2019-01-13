package io.epifab.yadl.domain

import shapeless.{::, Generic, HList, HNil, Lazy}

trait Terms[V] {
  type C
  def source: C
  def toSeq: Seq[Term[_]]
  def extractor: Extractor[V]
}

object Terms {
  type Aux[V, CX] = Terms[V] { type C = CX }

  def apply[C, V](t: C)(implicit readerBuilder: TermsBuilder.Aux[C, V]): Terms[V] =
    readerBuilder.build(t)
}

trait TermsBuilder[-C] {
  type Output
  def build(c: C): Terms[Output]
}

class TermBuilder[T] extends TermsBuilder[Term[T]] {
  override type Output = T
  override def build(c: Term[T]): Terms[Output] = new Terms[Output] {
    override type C = Term[T]
    override def source: C = c
    override def toSeq: Seq[Term[_]] = Seq(c)
    override def extractor: Extractor[T] = (row: Row) => row.get(source)
  }
}

class HigherOrderTermsBuilder[V, C] extends TermsBuilder[Terms[V]] {
  override type Output = V
  override def build(c: Terms[V]): Terms[V] = c
}

object HNilTermsBuilder extends TermsBuilder[HNil] {
  override type Output = HNil
  override def build(c: HNil): Terms[HNil] = new Terms[Output] {
    override type C = HNil
    override def source: C = c
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
    private val headReader = headBuilder.value.build(c.head)
    private val tailReader = tailBuilder.build(c.tail)
    override type C = HC :: TC
    override def source: C = c
    override def toSeq: Seq[Term[_]] = headReader.toSeq ++ tailReader.toSeq
    override def extractor: Extractor[H :: T] = (row: Row) => for {
      h <- headReader.extractor.extract(row)
      t <- tailReader.extractor.extract(row)
    } yield ::(h, t)
  }
}

class CaseClassTermsBuilder[CaseClass, GenericRepr, CX](implicit genericBuilder: TermsBuilder.Aux[CX, GenericRepr], gen: Generic.Aux[CaseClass, GenericRepr]) extends TermsBuilder[CX] {
  override type Output = CaseClass
  override def build(c: CX): Terms[CaseClass] = new Terms[Output] {
    override type C = CX
    override def source: C = c
    override def toSeq: Seq[Term[_]] = genericBuilder.build(c).toSeq
    override def extractor: Extractor[CaseClass] = genericBuilder.build(c).extractor.map(gen.from)
  }
}


object TermsBuilder {
  type Aux[C, VX] = TermsBuilder[C] { type Output = VX }

  implicit def termBuilder[T]: TermsBuilder.Aux[Term[T], T] =
    new TermBuilder[T]

  implicit def hNilTermsBuilder: TermsBuilder.Aux[HNil, HNil] =
    HNilTermsBuilder

  implicit def hConsTermsBuilder[H, T <: HList, HC, TC <: HList](implicit headBuilder: TermsBuilder.Aux[HC, H], tailBuilder: TermsBuilder.Aux[TC, T]): TermsBuilder.Aux[HC :: TC, H :: T] =
    new HConsTermsBuilder

  implicit def caseClassTermsBuilder[CaseClass, GenericRepr, CX](implicit genericBuilder: TermsBuilder.Aux[CX, GenericRepr], gen: Generic.Aux[CaseClass, GenericRepr]): TermsBuilder.Aux[CX, CaseClass] =
    new CaseClassTermsBuilder

  implicit def higherOrderTermsBuilder[C, V]: TermsBuilder.Aux[Terms[V], V] =
    new HigherOrderTermsBuilder
}
