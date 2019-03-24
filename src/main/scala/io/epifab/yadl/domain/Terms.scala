package io.epifab.yadl.domain

import shapeless.{::, Generic, HList, HNil, Lazy}
import shapeless.syntax.std.tuple._

sealed trait EditableTerms[Output, Container <: HList] {
  def values(v: Output, terms: Container): Seq[ColumnValue[_]]
}

object EditableTerms {
  implicit def hNil: EditableTerms[HNil, HNil] = new EditableTerms[HNil, HNil] {
    override def values(v: HNil, terms: HNil): Seq[ColumnValue[_]] = Seq.empty
  }

  implicit def hCons[X, ValueHead <: Term[X], ContainerHead <: Column[X], ValueTail <: HList, ContainerTail <: HList](implicit editableTail: EditableTerms[ValueTail, ContainerTail]): EditableTerms[ValueHead :: ValueTail, ContainerHead :: ContainerTail] = new EditableTerms[ValueHead :: ValueTail, ContainerHead :: ContainerTail] {
    override def values(values: ValueHead :: ValueTail, terms: ContainerHead :: ContainerTail): Seq[ColumnValue[_]] =
      new ColumnValue(terms.head, values.head) +: editableTail.values(values.tail, terms.tail)
  }
}

trait Terms[Output] { self =>
  type Container
  def container: Container
  def toSeq: Seq[Term[_]]
  def extractor: Extractor[Output]

  def map[Output2](f: Output => Output2): Terms[Output2] = new Terms[Output2] {
    override type Container = self.Container
    override def container: Container = self.container
    override def toSeq: Seq[Term[_]] = self.toSeq
    override def extractor: Extractor[Output2] = self.extractor.map(f)
  }
}

object Terms {
  type Aux[Output, ContainerX] = Terms[Output] { type Container = ContainerX }

  def apply[Container, Output](c: Container)(implicit builder: TermsBuilder.Aux[Container, Output]): Terms[Output] =
    builder.build(c)

  def apply[V1, V2](t1: Terms[V1], t2: Terms[V2]): Terms[(V1, V2)] =
    Terms(t1 :: t2 :: HNil).map((v: V1 :: V2 :: HNil) => v.tupled)

  def apply[V1, V2, V3](t1: Terms[V1], t2: Terms[V2], t3: Terms[V3]): Terms[(V1, V2, V3)] =
    Terms(t1 :: t2 :: t3 :: HNil).map((v: V1 :: V2 :: V3 :: HNil) => v.tupled)
}

trait TermsBuilder[-Container] { self =>
  type Output
  def build(c: Container): Terms[Output]
  def map[Output2](f: Output => Output2): TermsBuilder[Container] = new TermsBuilder[Container] {
    override type Output = Output2
    override def build(c: Container): Terms[Output2] = self.build(c).map(f)
  }
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
    private val genericTerms: Terms[GenericRepr] = genericBuilder.build(c)

    override type Container = ContainerX
    override def container: Container = c
    override def toSeq: Seq[Term[_]] = genericBuilder.build(c).toSeq

    override def extractor: Extractor[CaseClass] = genericTerms.extractor.map(gen.from)
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
