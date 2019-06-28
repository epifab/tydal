package io.epifab.yadl.domain

import io.epifab.yadl.utils.Appender
import shapeless.HNil

sealed trait Statement

sealed trait SideEffect


object SelectV2 {
  import io.epifab.yadl.implicits._
  import shapeless.{::, HList, HNil}

  trait Alias[A]

  type AS[T <: DataSource[_], A] = T with Alias[A]

  trait DataSource[TERMS <: HList] {
    def `*`: TERMS

    def on(clause: this.type => BinaryExpr): Join[this.type] =
      new Join(this, clause(this))

    def as[A]: this.type with Alias[A] =
      this.asInstanceOf[this.type with Alias[A]]
  }

  abstract class Table[TERMS <: HList](val tableName: String) extends DataSource[TERMS]

  class Join[+DS <: DataSource[_]](val dataSource: DS, filter: BinaryExpr)

  class SubQuery[TERMS <: HList, SOURCES <: HList](val select: Select[TERMS, SOURCES]) extends DataSource[TERMS] {
    def `*`: TERMS = select.terms
  }

  sealed trait Select[TERMS <: HList, SOURCES <: HList] {
    def terms: TERMS
    def sources: SOURCES
  }

  trait DataSourceFinder[X, U] {
    def find(u: U): X
  }

  object DataSourceFinder {
    implicit def joinedFinder[X <: DataSource[_], T <: HList]: DataSourceFinder[X, Join[X] :: T] =
      (u: Join[X] :: T) => u.head.dataSource

    implicit def headFinder[X, T <: HList]: DataSourceFinder[X, X :: T] =
      (u: X :: T) => u.head

    implicit def tailFinder[X, H, T <: HList](implicit finder: DataSourceFinder[X, T]): DataSourceFinder[X, H :: T] =
      (u: H :: T) => finder.find(u.tail)
  }

  class DataSourceContext[SOURCES <: HList](sources: SOURCES) {
    def get[X](implicit dataSourceFinder: DataSourceFinder[X, SOURCES]): X =
      dataSourceFinder.find(sources)
  }

  trait EmptySelect extends Select[HNil, HNil] {
    override def terms: HNil = HNil
    override def sources: HNil = HNil

    def from[T <: DataSource[_] with Alias[_]](source: T): NonEmptySelect[HNil, T :: HNil] =
      new NonEmptySelect(terms, source :: HNil)
  }

  class NonEmptySelect[TERMS <: HList, SOURCES <: HList](val terms: TERMS, val sources: SOURCES)
      extends Select[TERMS, SOURCES] {
    def take[NEW_TERMS <: HList]
        (f: DataSourceContext[SOURCES] => NEW_TERMS):
        NonEmptySelect[NEW_TERMS, SOURCES] =
      new NonEmptySelect(f(new DataSourceContext(sources)), sources)

    def join[NEW_SOURCE <: DataSource[_] with Alias[_], SOURCE_RESULTS <: HList]
        (f: DataSourceContext[SOURCES] => Join[NEW_SOURCE])
        (implicit appender: Appender.Aux[SOURCES, Join[NEW_SOURCE], SOURCE_RESULTS]):
        NonEmptySelect[TERMS, SOURCE_RESULTS] =
      new NonEmptySelect(terms, appender.append(sources, f(new DataSourceContext(sources))))
  }

  object Select extends EmptySelect

  //////////////////////////////////////////////////////
  // examples
  //////////////////////////////////////////////////////

  class Students extends Table[Term[Int] :: Term[String] :: HNil]("students") {
    val id: Term[Int] = Value(1)
    val name: Term[String] = Value("John")

    val `*`: Term[Int] :: Term[String] :: HNil = id :: name :: HNil
  }

  object Students {
    def as[T]: Students AS T = (new Students).as[T]
  }

  class Exams extends Table[Term[Int] :: Term[Int] :: Term[Int] :: HNil]("exams") {
    val studentId: Term[Int] = Value(1)
    val courseId: Term[Int] = Value(1)
    val score: Term[Int] = Value(30)

    val `*`: Term[Int] :: Term[Int] :: Term[Int] :: HNil =
      studentId :: courseId :: score :: HNil
  }

  object Exams {
    def as[T]: Exams AS T = (new Exams).as[T]
  }

  class Courses extends Table[Term[Int] :: Term[String] :: HNil]("courses") {
    val id: Term[Int] = Value(1)
    val name: Term[String] = Value("Math")

    val `*`: Term[Int] :: Term[String] :: HNil =
      id :: name :: HNil
  }

  object Courses {
    def as[T]: Courses AS T = (new Courses).as[T]
  }

  val examsSelect: Select[Term[Int] :: Aggregation[Int, Option[Int]] :: HNil, AS[Exams, "e"] :: HNil] =
    Select
      .from(Exams.as["e"])
      .take(ctx => ctx.get[Exams AS "e"].studentId :: Max(ctx.get[Exams AS "e"].score) :: HNil)

  class ExamsSubQuery extends SubQuery(examsSelect) {
    val (id: Term[Int], score: Aggregation[Int, Option[Int]]) = select.terms.tupled
  }

  val studentsSelect: Select[Term[Int] :: Term[String] :: Term[Int] :: Term[String] :: Term[Int] :: Aggregation[Int, Option[Int]] :: HNil, Students with Alias["s"] :: Join[ExamsSubQuery with Alias["e"]] :: Join[Courses with Alias["c"]] :: HNil] =
    Select
      .from(Students.as["s"])
      .join(ctx => (new ExamsSubQuery).as["e"].on(_.id === ctx.get[Students AS "s"].id))
      .join(ctx => Courses.as["c"].on(_.id === ctx.get[ExamsSubQuery AS "e"].id))
      .take(ctx =>
        ctx.get[Students AS "s"].* ++
        ctx.get[Courses AS "c"].* ++
        ctx.get[ExamsSubQuery AS "e"].*)
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

  def inRange(offset: Long, limit: Int): Select[V]
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

    def inRange(offset: Long, limit: Int): Select[V] =
      copy(limit = Some(Limit(offset, limit)))
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
