package io.epifab.yadl.domain

import shapeless.HNil

sealed trait Statement

sealed trait SideEffect


object SelectV2 {
  import shapeless._

  trait DataSource[REPR <: HList] {
    def `*`: REPR
  }

  abstract class Table[TERMS <: HList](val tableName: String) extends DataSource[TERMS]

  class Sources[S <: HList](s: S) {
    def apply[T](implicit finder: Finder[T, S]): T = finder.find(s)
  }

  sealed trait Select[SOURCES <: HList, TERMS <: HList] extends DataSource[TERMS] {
    def sources: SOURCES
  }

  trait EmptySelect extends Select[HNil, HNil] {
    override def sources: HNil = HNil
    override def `*`: HNil = HNil

    def from[T <: DataSource[_]](source: T): NonEmptySelect[T :: HNil, HNil] =
      new NonEmptySelect(source :: HNil, *)
  }

  object Select extends EmptySelect

  class NonEmptySelect[SOURCES <: HList, TERMS <: HList](val sources: SOURCES, val `*`: TERMS)
      extends Select[SOURCES, TERMS] {
    def take[NEW_TERMS <: HList](f: Sources[SOURCES] => NEW_TERMS): NonEmptySelect[SOURCES, NEW_TERMS :: TERMS] =
      new NonEmptySelect(sources, f(new Sources(sources)) :: *)
  }

  trait Finder[X, U] {
    def find(u: U): X
  }

  object Finder {
    implicit def headFinder[X, T <: HList]: Finder[X, X :: T] =
      (u: X :: T) => u.head

    implicit def tailFinder[X, H, T <: HList](implicit finder: Finder[X, T]): Finder[X, H :: T] =
      (u: H :: T) => finder.find(u.tail)
  }


  //////////////////////////////////////////////////////
  // examples
  //////////////////////////////////////////////////////

  trait Alias
  trait E extends Alias
  trait S extends Alias

  class Students extends Table[Term[Int] :: Term[String] :: HNil]("students") {
    def id: Term[Int] = Value(1)
    def name: Term[String] = Value("John")

    def `*`: Term[Int] :: Term[String] :: HNil = id :: name :: HNil
  }

  class Exams extends Table[Term[Int] :: Term[String] :: Term[Int] :: HNil]("exams") {
    def studentId: Term[Int] = Value(1)
    def courseName: Term[String] = Value("Math")
    def score: Term[Int] = Value(30)

    def `*`: Term[Int] :: Term[String] :: Term[Int] :: HNil =
      studentId :: courseName :: score :: HNil
  }

  val examsSelect: Select[Exams with E :: HNil, (Aggregation[Int, Option[Int]] :: Term[Int] :: HNil) :: HNil] =
    Select
      .from(new Exams with E)
      .take(self => Max(self[Exams with E].score) :: self[Exams with E].studentId :: HNil)

  val studentsSelect: Select[Students with S :: HNil, (Term[Int] :: Term[String] :: HNil) :: HNil] =
    Select
      .from(new Students with S)
      .take(self => self[Students with S].*)
}


sealed trait Select[V] extends Statement {
  def dataSource: DataSource
  def terms: Terms[V]
  def groupedBy: Seq[Term[_]]
  def joins: Seq[Join]
  def filter: BinaryExpr
  def sort: Seq[Sort]
  def limit: Option[Limit]

  def take[V2](r1: Terms[V2]): Select[V2]

  def take[S2, T2](subQuery: SubQuery[S2, T2]): Select[S2]

  def leftJoin[T <: DataSource](relation: Relation[T]): Select[V]

  def innerJoin[T <: DataSource](relation: Relation[T]): Select[V]

  def crossJoin(dataSource: DataSource): Select[V]

  def groupBy(terms: Term[_]*): Select[V]

  def where(filter: BinaryExpr): Select[V]

  def sortBy(sort: Sort*): Select[V]

  def inRange(start: Int, stop: Int): Select[V]
}

object Select {
  protected final case class SelectImpl[V](
    dataSource: DataSource,
    terms: Terms[V],
    groupedBy: Seq[Term[_]] = Seq.empty,
    joins: Seq[Join] = Seq.empty,
    filter: BinaryExpr = BinaryExpr.empty,
    sort: Seq[Sort] = Seq.empty,
    limit: Option[Limit] = None
  ) extends Select[V] {

    def take[V2](r1: Terms[V2]): Select[V2] =
      copy(terms = r1)

    def take[S2, T2](subQuery: SubQuery[S2, T2]): Select[S2] =
      copy(terms = subQuery.*)

    def groupBy(terms: Term[_]*): Select[V] =
      copy(groupedBy = terms)

    def leftJoin[T <: DataSource](relation: Relation[T]): Select[V] =
      copy(joins = joins :+ LeftJoin(relation, relation.clause))

    def innerJoin[T <: DataSource](relation: Relation[T]): Select[V] =
      copy(joins = joins :+ InnerJoin(relation, relation.clause))

    def crossJoin(dataSource: DataSource): Select[V] =
      copy(joins = joins :+ CrossJoin(dataSource))

    def where(filter: BinaryExpr): Select[V] =
      copy(filter = this.filter and filter)

    def sortBy(sort: Sort*): Select[V] =
      copy(sort = this.sort ++ sort)

    def inRange(start: Int, stop: Int): Select[V] =
      copy(limit = Some(Limit(start, stop)))
  }

  protected final case class SelectBuilder[V](terms: Terms[V]) {
    def from(dataSource: DataSource): Select[V] = SelectImpl(dataSource, terms)
  }

  def apply[V1](terms: Terms[V1]): SelectBuilder[V1] = SelectBuilder(terms)

  def apply[V1, V2](t1: Terms[V1], t2: Terms[V2]): SelectBuilder[(V1, V2)] =
    SelectBuilder(Terms(t1, t2))

  def apply[V1, V2, V3](t1: Terms[V1], t2: Terms[V2], t3: Terms[V3]): SelectBuilder[(V1, V2, V3)] =
    SelectBuilder(Terms(t1, t2, t3))

  def from(dataSource: DataSource): Select[HNil] = SelectImpl(dataSource, Terms(HNil))
}

sealed trait Insert[T] extends Statement with SideEffect {
  def table: Table[T]
  def columnValues: Seq[ColumnValue[_]]

  def set(t: T): Insert[T]
  def set(columnValues: ColumnValue[_]*): Insert[T]
}

object Insert {
  protected final case class InsertImpl[T](table: Table[T], columnValues: Seq[ColumnValue[_]] = Seq.empty) extends Insert[T] {
    def set(t: T): Insert[T] =
      copy(columnValues = table.*.values(t))

    def set(columnValues: ColumnValue[_]*): Insert[T] =
      copy(columnValues = columnValues)
  }

  def into[T](table: Table[T]) = InsertImpl(table)
}

sealed trait Update[T] extends Statement with SideEffect {
  def table: Table[T]
  def values: Seq[ColumnValue[_]]
  def filter: BinaryExpr

  def set(columnValues: ColumnValue[_]*): Update[T]
  def set(t: T): Update[T]

  def where(filter: BinaryExpr): Update[T]
}

object Update {
  protected final case class UpdateImpl[T](table: Table[T], values: Seq[ColumnValue[_]] = Seq.empty, filter: BinaryExpr = BinaryExpr.empty) extends Update[T] {
    def set(columnValues: ColumnValue[_]*): Update[T] =
      copy(values = columnValues)
    def set(t: T): Update[T] =
      copy(values = table.*.values(t))

    def where(filter: BinaryExpr): Update[T] =
      copy(filter = this.filter and filter)
  }

  def apply[T](table: Table[T]) = UpdateImpl(table)
}

sealed trait Delete extends Statement with SideEffect {
  def table: Table[_]
  def filter: BinaryExpr

  def where(filter: BinaryExpr): Delete
}

object Delete {
  protected final case class DeleteImpl(table: Table[_], filter: BinaryExpr = BinaryExpr.empty) extends Delete {
    def where(filter: BinaryExpr): Delete =
      copy(filter = this.filter and filter)
  }

  def apply(table: Table[_]) = DeleteImpl(table)
}
