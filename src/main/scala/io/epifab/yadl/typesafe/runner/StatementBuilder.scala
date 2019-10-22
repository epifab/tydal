package io.epifab.yadl.typesafe.runner

import java.sql.{Connection, PreparedStatement}

import io.epifab.yadl.typesafe.fields._
import io.epifab.yadl.typesafe.{Query, Tag, Tagged}
import shapeless.{::, HList, HNil}

/**
 * Type class to create a prepared statement
 *
 * @tparam CONNECTION Connection
 * @tparam STATEMENT Statement
 * @tparam PLACEHOLDER_VALUE Placeholder value
 */
trait StatementBuilder[CONNECTION, STATEMENT, PLACEHOLDER_VALUE] {
  def build(connection: CONNECTION, statement: STATEMENT, query: Query[_, _], input: PLACEHOLDER_VALUE): Unit
}

object StatementBuilder {
  implicit def jdbc[V <: Value[_] with Tag[_], A <: String, T]
      (implicit tagged: Tagged[V, A], valueT: ValueT[V, T]): StatementBuilder[Connection, PreparedStatement, V] =
    new StatementBuilder[Connection, PreparedStatement, V] {
      override def build(connection: Connection, statement: PreparedStatement, query: Query[_, _], input: V): Unit = {
//        query.placeholders.zipWithIndex.foreach {
//          case (placeholder, index) if placeholder.name == input.tagValue =>
//            set(connection, statement, index + 1, placeholder.encoder.dbType, input.dbValue)
//          case _ =>
//        }
      }

      def setSeq[U](connection: Connection, statement: PreparedStatement, index: Int, dbType: FieldType[U], value: Seq[U]): Unit = {
        val array: java.sql.Array = connection.createArrayOf(
          dbType.sqlName,
          value.toArray
        )
        statement.setArray(index, array)
      }

      @scala.annotation.tailrec
      def set[U, X](connection: Connection, statement: PreparedStatement, index: Int, dbType: FieldType[U], value: X): Unit = {
        dbType match {
          case TypeString | TypeDate | TypeDateTime | TypeJson | TypeEnum(_) | TypeGeography | TypeGeometry =>
            statement.setObject(index, value)

          case TypeInt =>
            statement.setInt(index, value.asInstanceOf[Int])

          case TypeDouble =>
            statement.setDouble(index, value.asInstanceOf[Double])

          case TypeSeq(innerType) =>
            setSeq(connection, statement, index, innerType, value.asInstanceOf[Seq[_]])

          case TypeOption(innerDbType) =>
            value match {
              case Some(v) => set(connection, statement, index, innerDbType, v)
              case None => statement.setObject(index, null)
            }
        }
      }
    }

  implicit def hNil[C, S]: StatementBuilder[C, S, HNil] =
    (connection: C, statement: S, query: Query[_, _], input: HNil) => { }

  implicit def hCons[C, S, H, T <: HList](implicit head: StatementBuilder[C, S, H], tail: StatementBuilder[C, S, T]): StatementBuilder[C, S, H :: T] =
    (connection: C, statement: S, query: Query[_, _], input: H :: T) => {
      head.build(connection, statement, query, input.head)
      tail.build(connection, statement, query, input.tail)
    }
}
