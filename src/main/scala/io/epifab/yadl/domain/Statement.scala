package io.epifab.yadl.domain

import io.epifab.yadl.utils.Appender
import shapeless.HNil

sealed trait Statement

sealed trait SideEffect


object SelectV2 {
  import io.epifab.yadl.implicits._
  import shapeless.{::, HList, HNil}

  trait DataSource[TERMS <: HList] {
    def `*`: TERMS
    def on(clause: this.type => BinaryExpr): Join[TERMS, this.type] = new Join(this, clause(this))
  }

  abstract class Table[TERMS <: HList](val tableName: String) extends DataSource[TERMS]

  class Join[TERMS <: HList, +DS <: DataSource[TERMS]](val dataSource: DS, filter: BinaryExpr) extends DataSource[TERMS] {
    def `*`: TERMS = dataSource.*
  }

  class SubQuery[SOURCES <: HList, TERMS <: HList](val select: Select[SOURCES, TERMS]) extends DataSource[TERMS] {
    def `*`: TERMS = select.terms
  }

  sealed trait Select[SOURCES <: HList, TERMS <: HList] {
    def sources: SOURCES
    def terms: TERMS
  }

  trait EmptySelect extends Select[HNil, HNil] {
    override def sources: HNil = HNil
    override def terms: HNil = HNil

    def from[T <: DataSource[_]](source: T): NonEmptySelect[T :: HNil, HNil] =
      new NonEmptySelect(source :: HNil, terms)
  }

  class NonEmptySelect[SOURCES <: HList, TERMS <: HList](val sources: SOURCES, val terms: TERMS)
      extends Select[SOURCES, TERMS] {
    def take[NEW_TERMS <: HList]
        (f: SOURCES => NEW_TERMS):
        NonEmptySelect[SOURCES, NEW_TERMS] =
      new NonEmptySelect(sources, f(sources))

    def join[JOIN_TERMS <: HList, NEW_SOURCE <: DataSource[JOIN_TERMS], SOURCE_RESULTS <: HList]
        (f: SOURCES => Join[JOIN_TERMS, NEW_SOURCE])
        (implicit appender: Appender.Aux[SOURCES, Join[JOIN_TERMS, NEW_SOURCE], SOURCE_RESULTS]):
        NonEmptySelect[SOURCE_RESULTS, TERMS] =
      new NonEmptySelect(appender.append(sources, f(sources)), terms)
  }

  object Select extends EmptySelect

  //////////////////////////////////////////////////////
  // examples
  //////////////////////////////////////////////////////

  import language.implicitConversions
  implicit def joinToDataSource[TERMS <: HList, DS <: DataSource[TERMS]](join: Join[TERMS, DS]): DS = join.dataSource

  class Students extends Table[Term[Int] :: Term[String] :: HNil]("students") {
    val id: Term[Int] = Value(1)
    val name: Term[String] = Value("John")

    val `*`: Term[Int] :: Term[String] :: HNil = id :: name :: HNil
  }

  class Exams extends Table[Term[Int] :: Term[Int] :: Term[Int] :: HNil]("exams") {
    val studentId: Term[Int] = Value(1)
    val courseId: Term[Int] = Value(1)
    val score: Term[Int] = Value(30)

    val `*`: Term[Int] :: Term[Int] :: Term[Int] :: HNil =
      studentId :: courseId :: score :: HNil
  }

  class Course extends Table[Term[Int] :: Term[String] :: HNil]("courses") {
    val id: Term[Int] = Value(1)
    val name: Term[String] = Value("Math")

    val `*`: Term[Int] :: Term[String] :: HNil =
      id :: name :: HNil
  }

  val examsSelect: Select[Exams :: HNil, Term[Int] :: Aggregation[Int, Option[Int]] :: HNil] =
    Select
      .from(new Exams)
      .take { case exams :: HNil => exams.studentId :: Max(exams.score) :: HNil }

  class ExamsSubQuery extends SubQuery(examsSelect) {
    val (id: Term[Int], score: Aggregation[Int, Option[Int]]) = select.terms.tupled
  }

  trait Alias
  trait S1 extends Alias
  trait E1 extends Alias

  val studentsSelect: Select[Students :: Join[Term[Int] :: Aggregation[Int, Option[Int]] :: HNil, ExamsSubQuery] :: Join[Term[Int] :: Term[String] :: HNil, Course] :: HNil, Term[Int] :: Term[String] :: Term[Int] :: Term[String] :: Term[Int] :: Aggregation[Int, Option[Int]] :: HNil] =
    Select
      .from(new Students)
      .join { case students :: HNil =>
        (new ExamsSubQuery).on(_.id === students.id) }
      .join { case _ :: exams :: HNil =>
        (new Course).on(_.id === exams.id) }
      .take { case students :: exams :: course :: HNil => students.* ++ course.* ++ exams.* }
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
