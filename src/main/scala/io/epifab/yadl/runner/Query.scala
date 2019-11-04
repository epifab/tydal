package io.epifab.yadl.runner

import io.epifab.yadl.fields._
import io.epifab.yadl.utils.Concat
import io.epifab.yadl._
import shapeless.{::, Generic, HList, HNil}

case class Query[+PLACEHOLDERS <: HList, +FIELDS <: HList](sql: String, placeholders: PLACEHOLDERS, fields: FIELDS)

case class QueryFragment[P <: HList](sql: Option[String], placeholders: P) {
  def `++`(s: String): QueryFragment[P] =
    append(s)

  def `++`[Q <: HList, R <: HList](other: QueryFragment[Q])(implicit concat: Concat.Aux[P, Q, R]): QueryFragment[R] =
    concatenateOptional(other, "")

  def `+ +`[Q <: HList, R <: HList](other: QueryFragment[Q])(implicit concat: Concat.Aux[P, Q, R]): QueryFragment[R] =
    concatenateOptional(other, " ")

  def `+,+`[Q <: HList, R <: HList](other: QueryFragment[Q])(implicit concat: Concat.Aux[P, Q, R]): QueryFragment[R] =
    concatenateOptional(other, ", ")

  def wrap(before: String, after: String): QueryFragment[P] =
    QueryFragment(sql.map(before + _ + after), placeholders)

  def append(after: String): QueryFragment[P] =
    QueryFragment(sql.map(_ + after), placeholders)

  def prepend(before: String): QueryFragment[P] =
    QueryFragment(sql.map(before + _), placeholders)

  def map(f: String => String): QueryFragment[P] =
    QueryFragment(sql.map(f), placeholders)

  def concatenateOptional[Q <: HList, R <: HList](other: QueryFragment[Q], separator: String)(implicit concat: Concat.Aux[P, Q, R]): QueryFragment[R] =
    QueryFragment.apply(
      Seq(sql, other.sql)
        .flatten
        .reduceOption(_ ++ separator ++ _),
      concat(placeholders, other.placeholders)
    )

  def concatenateRequired[Q <: HList, R <: HList](other: QueryFragment[Q], separator: String)(implicit concat: Concat.Aux[P, Q, R]): QueryFragment[R] =
    QueryFragment.apply(
      for {
        s1 <- sql
        s2 <- other.sql
      } yield s1 + separator + s2,
      concat(placeholders, other.placeholders)
    )

  def getOrElse[FIELDS <: HList](default: String, fields: FIELDS): Query[P, FIELDS] =
    Query(sql.getOrElse(default), placeholders, fields)

  def orElse(s: Option[String]): QueryFragment[P] =
    new QueryFragment(sql.orElse(s), placeholders)

  def get[FIELDS <: HList](fields: FIELDS): Query[P, FIELDS] =
    Query(sql.get, placeholders, fields)
}

object QueryFragment {
  def empty: QueryFragment[HNil] = QueryFragment(None, HNil)

  def apply[P <: HList](query: Query[P, _]): QueryFragment[P] =
    QueryFragment(Some(query.sql), query.placeholders)

  def apply(sql: String): QueryFragment[HNil] =
    QueryFragment(Some(sql), HNil)

  def apply[P <: Placeholder[_]](sql: String, placeholder: P): QueryFragment[P :: HNil] =
    QueryFragment(Some(sql), placeholder :: HNil)
}

sealed trait QueryBuilder[-X, P <: HList, F <: HList] {
  def build(x: X): Query[P, F]
}

object QueryBuilder {
  def apply[X, P <: HList, F <: HList](x: X)(implicit queryBuilder: QueryBuilder[X, P, F]): Query[P, F] =
    queryBuilder.build(x)

  def instance[T, P <: HList, F <: HList](f: T => Query[P, F]): QueryBuilder[T, P, F] =
    new QueryBuilder[T, P, F] {
      override def build(x: T): Query[P, F] = f(x)
    }

  implicit def selectQuery[FIELDS <: HList, GROUP_BY <: HList, SOURCES <: HList, WHERE <: BinaryExpr, P1 <: HList, P2 <: HList, P3 <: HList, P4 <: HList, P5 <: HList, P6 <: HList, P7 <: HList]
    (implicit
     fields: QueryFragmentBuilder["fields", FIELDS, P1],
     from: QueryFragmentBuilder["from", SOURCES, P2],
     concat1: Concat.Aux[P1, P2, P3],
     where: QueryFragmentBuilder["where", WHERE, P4],
     concat2: Concat.Aux[P3, P4, P5],
     groupBy: QueryFragmentBuilder["groupBy", GROUP_BY, P6],
     concat3: Concat.Aux[P5, P6, P7]): QueryBuilder[Select[FIELDS, GROUP_BY, SOURCES, WHERE], P7, FIELDS] =
    QueryBuilder.instance(select =>
      (fields.build(select.fields).orElse(Some("1")).prepend("SELECT ") `+ +`
        from.build(select.sources).map("FROM " + _) `+ +`
        where.build(select.filter).map("WHERE " + _) `+ +`
        groupBy.build(select.groupByFields).map("GROUP BY " + _)
      ).get(select.fields)
    )

  implicit def insertQuery[NAME <: String, SCHEMA, COLUMNS <: HList, PLACEHOLDERS <: HList]
      (implicit
       generic: Generic.Aux[SCHEMA, COLUMNS],
       names: QueryFragmentBuilder["name", COLUMNS, HNil],
       placeholders: QueryFragmentBuilder["placeholder", COLUMNS, PLACEHOLDERS]): QueryBuilder[Insert[NAME, SCHEMA], PLACEHOLDERS, HNil] =
    QueryBuilder.instance(insert => {
      val columns = generic.to(insert.table.schema)
      (names.build(columns).wrap("(", ")") ++
        " VALUES " ++
        placeholders.build(columns).wrap("(", ")")
      ).prepend("INSERT INTO " + insert.table.tableName + " ").get(HNil)
    })

  implicit def updateQuery[NAME <: String, SCHEMA, COLUMNS <: HList, P <: HList, Q <: HList, R <: HList, WHERE <: BinaryExpr]
      (implicit
       placeholders: QueryFragmentBuilder["name=placeholder", COLUMNS, P],
       where: QueryFragmentBuilder["where", WHERE, Q],
       concat: Concat.Aux[P, Q, R]
      ): QueryBuilder[Update[NAME, SCHEMA, COLUMNS, WHERE], R, HNil] =
    QueryBuilder.instance(update => {
      (placeholders.build(update.fields).prepend("UPDATE " + update.table.tableName + " SET ") ++
        where.build(update.where).prepend(" WHERE ")).get(HNil)
    })

  implicit def deleteQuery[NAME <: String, SCHEMA, P <: HList, WHERE <: BinaryExpr]
      (implicit
       where: QueryFragmentBuilder["where", WHERE, P]
      ): QueryBuilder[Delete[NAME, SCHEMA, WHERE], P, HNil] =
    QueryBuilder.instance(delete =>
      (QueryFragment(s"DELETE FROM ${delete.table.tableName}") ++
      where.build(delete.filter).prepend(" WHERE "))
          .get(HNil)
    )
}

sealed trait QueryFragmentBuilder[TYPE, -X, P <: HList] {
  def build(x: X): QueryFragment[P]
}

object QueryFragmentBuilder {
  def instance[A <: String, T, P <: HList](f: T => QueryFragment[P]): QueryFragmentBuilder[A, T, P] =
    new QueryFragmentBuilder[A, T, P] {
      override def build(x: T): QueryFragment[P] = f(x)
    }

  implicit def emptySourceFrom: QueryFragmentBuilder["from", HNil, HNil] =
    QueryFragmentBuilder.instance(_ => QueryFragment.empty)

  implicit def nonEmptySourceFrom[H, T <: HList, P <: HList, Q <: HList, R <: HList]
      (implicit
       head: QueryFragmentBuilder["from", H, P],
       tail: QueryFragmentBuilder["from", T, Q],
       concat: Concat.Aux[P, Q, R]): QueryFragmentBuilder["from", H :: T, R] =
    instance { sources =>
      head.build(sources.head) `+ +` tail.build(sources.tail)
    }

  implicit def joinFrom[S <: Selectable[_] with Tag[_], WHERE <: BinaryExpr, P <: HList, Q <: HList, R <: HList]
      (implicit
       src: QueryFragmentBuilder["from", S, P],
       where: QueryFragmentBuilder["where", WHERE, Q],
       concat: Concat.Aux[P, Q, R]): QueryFragmentBuilder["from", Join[S, WHERE], R] =
    instance(join =>
      src.build(join.selectable).prepend("INNER JOIN ") ++
        where.build(join.filter).prepend(" ON ")
    )

  implicit val tableFrom: QueryFragmentBuilder["from", Table[_, _] with Tag[_], HNil] =
    QueryFragmentBuilder.instance(table => QueryFragment(table.tableName + " AS " + table.tagValue))

  implicit def subQueryFrom[SUBQUERY_FIELDS <: HList, S <: Select[_, _, _, _], P <: HList]
      (implicit query: QueryBuilder[S, P, _]): QueryFragmentBuilder["from", SelectSubQuery[SUBQUERY_FIELDS, S] with Tag[_], P] =
    instance(subQuery =>
      QueryFragment(query.build(subQuery.select))
        .wrap("(", ") AS " + subQuery.tagValue)
    )

  // ------------------------------
  // Fields
  // ------------------------------

  implicit def field[P <: HList, F <: Field[_] with Tag[_]](implicit src: QueryFragmentBuilder["src", F, P]): QueryFragmentBuilder["fields", F, P] =
    instance((f: F) => src.build(f).append(" AS " + f.tagValue))

  implicit def emptyFields: QueryFragmentBuilder["fields", HNil, HNil] =
    QueryFragmentBuilder.instance(_ => QueryFragment.empty)

  implicit def nonEmptyFields[H, T <: HList, P <: HList, Q <: HList, R <: HList]
      (implicit
       head: QueryFragmentBuilder["fields", H, P],
       tail: QueryFragmentBuilder["fields", T, Q],
       concat: Concat.Aux[P, Q, R]): QueryFragmentBuilder["fields", H :: T, R] =
    QueryFragmentBuilder.instance(fields =>
      head.build(fields.head) `+,+` tail.build(fields.tail)
    )

  // ------------------------------
  // Group by
  // ------------------------------

  implicit def groupByField[F <: Field[_], P <: HList](implicit field: QueryFragmentBuilder["src", F, P]): QueryFragmentBuilder["groupBy", F, P] =
    instance(field.build)

  implicit def emptyGroupByFields: QueryFragmentBuilder["groupBy", HNil, HNil] =
    QueryFragmentBuilder.instance(_ => QueryFragment.empty)

  implicit def nonEmptyGroupByFields[H, T <: HList, P <: HList, Q <: HList, R <: HList]
      (implicit
       head: QueryFragmentBuilder["groupBy", H, P],
       tail: QueryFragmentBuilder["groupBy", T, Q],
       concat: Concat.Aux[P, Q, R]): QueryFragmentBuilder["groupBy", H :: T, R] =
    QueryFragmentBuilder.instance(fields =>
      head.build(fields.head) `+,+` tail.build(fields.tail)
    )

  // ------------------------------
  // Field (src)
  // ------------------------------

  implicit val columnSrc: QueryFragmentBuilder["src", Column[_], HNil] =
    instance((f: Column[_]) => QueryFragment(s"${f.srcAlias}.${f.name}"))

  implicit def castSrc[F <: Field[_], P <: HList](implicit builder: QueryFragmentBuilder["src", F, P]): QueryFragmentBuilder["src", Cast[F, _], P] =
    instance((f: Cast[F, _]) => builder.build(f.field).append("::" + f.decoder.dbType.sqlName))

  implicit def nullableSrc[F <: Field[_], P <: HList](implicit builder: QueryFragmentBuilder["src", F, P]): QueryFragmentBuilder["src", Nullable[F, _], P] =
    instance((f: Nullable[F, _]) => builder.build(f.field))

  implicit def aggregationSrc[F <: Field[_], P <: HList](implicit builder: QueryFragmentBuilder["src", F, P]): QueryFragmentBuilder["src", Aggregation[F, _], P] =
    instance((f: Aggregation[F, _]) => builder.build(f.field).wrap(f.dbFunction.name + "(", ")"))

  implicit def fieldExpr1[F <: Field[_], P <: HList](implicit builder: QueryFragmentBuilder["src", F, P]): QueryFragmentBuilder["src", FieldExpr1[F, _], P] =
    instance((f: FieldExpr1[F, _]) => builder.build(f.field).wrap(f.dbFunction.name + "(", ")"))

  implicit def fieldExpr2[F1 <: Field[_], F2 <: Field[_], P <: HList, Q <: HList, R <: HList]
      (implicit
       builder1: QueryFragmentBuilder["src", F1, P],
       builder2: QueryFragmentBuilder["src", F2, Q],
       concat: Concat.Aux[P, Q, R]): QueryFragmentBuilder["src", FieldExpr2[F1, F2, _], R] =
    instance((f: FieldExpr2[F1, F2, _]) => (builder1.build(f.field1) `+,+` builder2.build(f.field2)).wrap(f.dbFunction.name + "(", ")"))

  implicit def placeholder[P <: NamedPlaceholder[_]]: QueryFragmentBuilder["src", P, P :: HNil] =
    instance((f: P) => QueryFragment(s"?::${f.encoder.dbType.sqlName}", f))

  implicit def value[V <: PlaceholderValue[_]]: QueryFragmentBuilder["src", V, V :: HNil] =
    instance((f: V) => QueryFragment(s"?::${f.encoder.dbType.sqlName}", f))

  implicit def optionalValue[V <: OptionalPlaceholderValue[_]]: QueryFragmentBuilder["src", V, V :: HNil] =
    instance((f: V) => QueryFragment(f.value.map(_ => s"?::${f.encoder.dbType.sqlName}"), f :: HNil))

  // ------------------------------
  // Column name and placeholder
  // ------------------------------

  implicit val columnName: QueryFragmentBuilder["name", Column[_], HNil] =
    instance((f: Column[_]) => QueryFragment(f.name))

  implicit def emptyColumnNames: QueryFragmentBuilder["name", HNil, HNil] =
    QueryFragmentBuilder.instance(_ => QueryFragment.empty)

  implicit def nonEmptyColumnNames[H, T <: HList, P <: HList, Q <: HList, R <: HList]
      (implicit
       head: QueryFragmentBuilder["name", H, P],
       tail: QueryFragmentBuilder["name", T, Q],
       concat: Concat.Aux[P, Q, R]): QueryFragmentBuilder["name", H :: T, R] =
    instance { sources =>
      head.build(sources.head) `+,+` tail.build(sources.tail)
    }

  implicit def columnPlaceholder[A <: String, T, PL <: HList]
      (implicit
       tag: ValueOf[A],
       fieldDecoder: FieldDecoder[T],
       fieldEncoder: FieldEncoder[T],
       queryFragmentBuilder: QueryFragmentBuilder["src", NamedPlaceholder[T] with Tag[A], PL]
      ): QueryFragmentBuilder["placeholder", Column[T] with Tag[A], PL] =
    instance((_: Column[T] with Tag[A]) => queryFragmentBuilder.build(NamedPlaceholder[T, A]))

  implicit def emptyColumnPlaceholders: QueryFragmentBuilder["placeholder", HNil, HNil] =
    QueryFragmentBuilder.instance(_ => QueryFragment.empty)

  implicit def nonEmptyColumnPlaceholders[H, T <: HList, P <: HList, Q <: HList, R <: HList]
      (implicit
       head: QueryFragmentBuilder["placeholder", H, P],
       tail: QueryFragmentBuilder["placeholder", T, Q],
       concat: Concat.Aux[P, Q, R]): QueryFragmentBuilder["placeholder", H :: T, R] =
    instance { sources =>
      head.build(sources.head) `+,+` tail.build(sources.tail)
    }

  implicit def columnNameAndPlaceholder[A <: String, T, PL <: HList]
      (implicit
       tag: ValueOf[A],
       fieldDecoder: FieldDecoder[T],
       fieldEncoder: FieldEncoder[T],
       queryFragmentBuilder: QueryFragmentBuilder["src", NamedPlaceholder[T] with Tag[A], PL]
      ): QueryFragmentBuilder["name=placeholder", Column[T] with Tag[A], PL] =
    instance((_: Column[T] with Tag[A]) =>
      queryFragmentBuilder.build(NamedPlaceholder[T, A])
        .prepend(tag.value + " = "))

  implicit def emptyColumnNameAndPlaceholders: QueryFragmentBuilder["name=placeholder", HNil, HNil] =
    QueryFragmentBuilder.instance(_ => QueryFragment.empty)

  implicit def nonEmptyColumnNameAndPlaceholders[H, T <: HList, P <: HList, Q <: HList, R <: HList]
      (implicit
       head: QueryFragmentBuilder["name=placeholder", H, P],
       tail: QueryFragmentBuilder["name=placeholder", T, Q],
       concat: Concat.Aux[P, Q, R]): QueryFragmentBuilder["name=placeholder", H :: T, R] =
    instance { sources =>
      head.build(sources.head) `+,+` tail.build(sources.tail)
    }

  // ------------------------------
  // Binary expressions
  // ------------------------------

  implicit def whereAlwaysTrue: QueryFragmentBuilder["where", AlwaysTrue, HNil] =
    instance((_: AlwaysTrue) => QueryFragment.empty)

  implicit def binaryExpr1[E1 <: BinaryExpr, P <: HList]
      (implicit left: QueryFragmentBuilder["where", E1, P]): QueryFragmentBuilder["where", BinaryExpr1[E1], P] =
    instance { (expr: BinaryExpr1[E1]) =>
      val e1 = left.build(expr.expr)
      expr match {
        case _: IsDefined[_] => e1 ++ " IS NOT NULL"
        case _: IsNotDefined[_] => e1 ++ " IS NULL"
      }
    }

  implicit def binaryExpr2[E1, E2, P <: HList, Q <: HList, R <: HList]
      (implicit
       left: QueryFragmentBuilder["where", E1, P],
       right: QueryFragmentBuilder["where", E2, Q],
       concat: Concat.Aux[P, Q, R]): QueryFragmentBuilder["where", BinaryExpr2[E1, E2], R] =
    instance { (expr: BinaryExpr2[E1, E2]) =>
      val e1 = left.build(expr.expr1)
      val e2 = right.build(expr.expr2)
      expr match {
        case _: And[_, _] => e1.concatenateOptional(e2, " AND ")
        case _: Or[_, _] => e1.concatenateOptional(e2, " OR ")
        case _: Equals[_, _] => e1.concatenateRequired(e2, " = ")
        case _: NotEquals[_, _] => e1.concatenateRequired(e2, " != ")
        case _: GreaterThan[_, _] => e1.concatenateRequired(e2, " > ")
        case _: LessThan[_, _] => e1.concatenateRequired(e2, " < ")
        case _: GreaterThanOrEqual[_, _] => e1.concatenateRequired(e2, " >= ")
        case _: LessThanOrEqual[_, _] => e1.concatenateRequired(e2, " <= ")
        case _: Like[_, _] => e1.concatenateRequired(e2, " LIKE ")
        case _: IsSubset[_, _] => e1.concatenateRequired(e2, " <@ ")
        case _: IsSuperset[_, _] => e1.concatenateRequired(e2, " @> ")
        case _: Overlaps[_, _] => e1.concatenateRequired(e2, " && ")
        case _: IsIncluded[_, _] => e1.concatenateRequired(e2.wrap("(", ")"), " = ANY")
      }
    }

  implicit def whereField[P <: HList, F <: Field[_]](implicit field: QueryFragmentBuilder["src", F, P]): QueryFragmentBuilder["where", F, P] =
    instance(field.build)
}
