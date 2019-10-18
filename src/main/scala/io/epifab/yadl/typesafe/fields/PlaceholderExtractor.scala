package io.epifab.yadl.typesafe.fields

import io.epifab.yadl.typesafe._
import io.epifab.yadl.typesafe.utils.Concat
import shapeless.{::, HList, HNil}

sealed trait RemoveElement[NEEDLE, HAYSTACK, RESULTS <: HList] {
  def remove(haystack: HAYSTACK): RESULTS
}

object RemoveElement {
  class PartiallyAppliedRemoveElement[NEEDLE] {
    def apply[HAYSTACK, RESULTS <: HList](b: HAYSTACK)(implicit removeByTag: RemoveElement[NEEDLE, HAYSTACK, RESULTS]): RESULTS =
      removeByTag.remove(b)
  }

  def apply[NEEDLE]: PartiallyAppliedRemoveElement[NEEDLE] = new PartiallyAppliedRemoveElement[NEEDLE]

  sealed trait SameElement[+A, -B]

  object SameElement {
    def apply[A, B](implicit sameElement: SameElement[A, B]): Unit = {}

    implicit def sameElement[A]: SameElement[A, A] =  new SameElement[A, A] {}
  }

  trait DifferentElement[A, B]

  object DifferentElement {
    def apply[A, B](implicit differentElement: DifferentElement[A, B]): Unit = {}

    implicit def actuallyTheSame[A, B](implicit sameElement: SameElement[A, B]): DifferentElement[A, B] =
      new DifferentElement[A, B] {}

    implicit def differentElement[A, B]: DifferentElement[A, B] =
      new DifferentElement[A, B] {}
  }

  implicit def removed[A <: Tag[_], B <: Tag[_]](implicit sameElement: SameElement[A, B]): RemoveElement[A, B, HNil] = new RemoveElement[A, B, HNil] {
    override def remove(haystack: B): HNil = HNil
  }

  implicit def notRemoved[A <: Tag[_], B <: Tag[_]](implicit differentElement: DifferentElement[A, B]): RemoveElement[A, B, B :: HNil] = new RemoveElement[A, B, B :: HNil] {
    override def remove(haystack: B): B :: HNil = haystack :: HNil
  }

  implicit def hNil[A]: RemoveElement[A, HNil, HNil] = new RemoveElement[A, HNil, HNil] {
    override def remove(haystack: HNil): HNil = HNil
  }

  implicit def hCons[A, H, HX <: HList, T <: HList, TX <: HList, XX <: HList]
    (implicit
     headRemover: RemoveElement[A, H, HX],
     tailRemover: RemoveElement[A, T, TX],
     concat: Concat.Aux[HX, TX, XX]): RemoveElement[A, H :: T, XX] = new RemoveElement[A, H :: T, XX] {
    override def remove(haystack: H :: T): XX =
      concat.concat(headRemover.remove(haystack.head), tailRemover.remove(haystack.tail))
  }
}

sealed trait HSet[SRC <: HList, TARGET <: HList] {
  def removeDuplicates(src: SRC): TARGET
}

object HSet {
  class PartiallyAppliedHSet[SRC <: HList](src: SRC) {
    def get[TARGET <: HList](implicit duplicateRemover: HSet[SRC, TARGET]): TARGET =
      duplicateRemover.removeDuplicates(src)
  }

  def apply[SRC <: HList](src: SRC): PartiallyAppliedHSet[SRC] = new PartiallyAppliedHSet(src)

  private def instance[A <: HList, B <: HList](f: A => B): HSet[A, B] = new HSet[A, B] {
    override def removeDuplicates(src: A): B = f(src)
  }

  implicit def hNil: HSet[HNil, HNil] =
    instance((_: HNil) => HNil)

  implicit def hCons[H <: Tag[_], T <: HList, HX <: HList, XX <: HList]
    (implicit
     firstRemover: RemoveElement[H, T, HX],
     secondRemover: HSet[HX, XX]): HSet[H :: T, H :: XX] =
    instance((list: H :: T) =>
      list.head :: secondRemover.removeDuplicates(firstRemover.remove(list.tail)))
}


sealed trait PlaceholderExtractor[-HAYSTACK, PLACEHOLDERS <: HList] {
  def extract(haystack: HAYSTACK): PLACEHOLDERS
}

object PlaceholderExtractor {
  def apply[HAYSTACK, PS <: HList](haystack: HAYSTACK)(implicit placeholderExtractor: PlaceholderExtractor[HAYSTACK, PS]): PS =
    placeholderExtractor.extract(haystack)

  def instance[H, P <: HList](f: H => P): PlaceholderExtractor[H, P] = new PlaceholderExtractor[H, P] {
    override def extract(haystack: H): P = f(haystack)
  }

  //---------------------------------------------------
  // Select
  //---------------------------------------------------

  implicit def select[FIELDS <: HList, GROUP_BY <: HList, SOURCES <: HList, WHERE <: BinaryExpr, X1 <: HList, X2 <: HList, X12 <: HList, X3 <: HList, X <: HList]
    (implicit
     fields: PlaceholderExtractor[FIELDS, X1],
     sources: PlaceholderExtractor[SOURCES, X2],
     where: PlaceholderExtractor[WHERE, X3],
     concat1: Concat.Aux[X1, X2, X12],
     concat2: Concat.Aux[X12, X3, X]
    ): PlaceholderExtractor[Select[FIELDS, GROUP_BY, SOURCES, WHERE], X] =
    instance((select: Select[FIELDS, GROUP_BY, SOURCES, WHERE]) => concat2.concat(
      concat1.concat(
        fields.extract(select.fields),
        sources.extract(select.sources)
      ),
      where.extract(select.filter)
    ))

  //------------- --------------------------------------
  // DataSource
  //---------------------------------------------------

  implicit def table: PlaceholderExtractor[Table[_, _], HNil] =
    instance((t: Table[_, _]) => HNil)

  implicit def subQuery[S <: Select[_, _, _, _], X <: HList]
    (implicit select: PlaceholderExtractor[S, X]): PlaceholderExtractor[SubQuery[_, S], X] =
    instance((subQuery: SubQuery[_, S]) => select.extract(subQuery.select))

  implicit def join[DS <: DataSource[_], X1 <: HList, WHERE <: BinaryExpr, X2 <: HList, X <: HList]
    (implicit
     dataSource: PlaceholderExtractor[DS, X1],
     where: PlaceholderExtractor[WHERE, X2],
     concat: Concat.Aux[X1, X2, X]): PlaceholderExtractor[Join[DS, WHERE], X] =
    instance((join: Join[DS, WHERE]) =>
      concat.concat(dataSource.extract(join.dataSource), where.extract(join.filter)))

  //---------------------------------------------------
  // Fields
  //---------------------------------------------------

  implicit def column: PlaceholderExtractor[Column[_], HNil] =
    instance((c: Column[_]) => HNil)

  implicit def aggregation[F <: Field[_], X <: HList](implicit placeholderExtractor: PlaceholderExtractor[F, X]): PlaceholderExtractor[Aggregation[F, _], X] =
    instance((aggregation: Aggregation[F, _]) => placeholderExtractor.extract(aggregation.field))

  implicit def cast[F <: Field[_], X <: HList](implicit placeholderExtractor: PlaceholderExtractor[F, X]): PlaceholderExtractor[Cast[F, _], X] =
    instance((cast: Cast[F, _]) => placeholderExtractor.extract(cast.field))

  implicit def fieldExpr1[F <: Field[_], X <: HList]
      (implicit placeholderExtractor: PlaceholderExtractor[F, X]): PlaceholderExtractor[FieldExpr1[F, _], X] =
    instance((expr: FieldExpr1[F, _]) => placeholderExtractor.extract(expr.field))

  implicit def fieldExpr2[F1 <: Field[_], F2 <: Field[_], X1 <: HList, X2 <: HList, X <: HList]
      (implicit
       f1: PlaceholderExtractor[F1, X1],
       f2: PlaceholderExtractor[F2, X2],
       concat: Concat.Aux[X1, X2, X]): PlaceholderExtractor[FieldExpr2[F1, F2, _], X] =
    instance((expr: FieldExpr2[F1, F2, _]) => concat.concat(f1.extract(expr.field1), f2.extract(expr.field2)))

  implicit def placeholder[P <: Placeholder[_, _]]: PlaceholderExtractor[P, P :: HNil] =
    instance((haystack: P) => haystack :: HNil)

  //---------------------------------------------------
  // BinaryExpr
  //---------------------------------------------------

  implicit def alwaysTrue: PlaceholderExtractor[AlwaysTrue, HNil] =
    instance((_: AlwaysTrue) => HNil)

  implicit def binaryExpr1[E1, X <: HList]
      (implicit e: PlaceholderExtractor[E1, X]): PlaceholderExtractor[BinaryExpr1[E1], X] =
    instance((haystack: BinaryExpr1[E1]) => e.extract(haystack.expr))

  implicit def binaryExpr2[E1, X1 <: HList, E2, X2 <: HList, X <: HList]
      (implicit
       e1: PlaceholderExtractor[E1, X1],
       e2: PlaceholderExtractor[E2, X2],
       concat: Concat.Aux[X1, X2, X]): PlaceholderExtractor[BinaryExpr2[E1, E2], X] =
    instance((haystack: BinaryExpr2[E1, E2]) =>
      concat.concat(e1.extract(haystack.expr1), e2.extract(haystack.expr2)))

  //---------------------------------------------------
  // Generic hlist
  //---------------------------------------------------

  implicit def hNil[NEEDLE]: PlaceholderExtractor[HNil, HNil] =
    instance((_: HNil) => HNil)

  implicit def hCons[H, T <: HList, HX <: HList, TX <: HList, XX <: HList]
      (implicit
       head: PlaceholderExtractor[H, HX],
       tail: PlaceholderExtractor[T, TX],
       concat: Concat.Aux[HX, TX, XX]): PlaceholderExtractor[H :: T, XX] =
    instance((haystack: H :: T) =>
      concat.concat(head.extract(haystack.head), tail.extract(haystack.tail)))
}
