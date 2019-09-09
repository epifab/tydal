package io.epifab.yadl.typesafe

import io.epifab.yadl.typesafe.fields._

object QueryBuilder {
  def fields[FIELDS](fields: FIELDS)(implicit tagMap: TagMap[Field[Any], FIELDS]): String =
    tagMap.toMap(fields)
      .map { case (alias, field) => build(field) + " AS " + alias }
      .mkString(", ")

  def build(field: Field[_]): String = field match {
    case Column(name, srcAlias) => s"$srcAlias.$name"
    case cast@ Cast(field) => build(field) + "::" + cast.decoder.dbType.sqlName
    case Aggregation(field, dbFunction) => dbFunction.name + "(" + build(field) + ")"
    case FieldExpr1(field, dbFunction) => dbFunction.name + "(" + build(field) + ")"
    case FieldExpr2(field1, field2, dbFunction) => dbFunction.name + "(" + build(field1) + "," + build(field2) + ")"
    case FieldExpr3(field1, field2, field3, dbFunction) => dbFunction.name + "(" + build(field1) + "," + build(field2) + "," + build(field3) + ")"
    case Placeholder(name) => s":$name"
  }
}
