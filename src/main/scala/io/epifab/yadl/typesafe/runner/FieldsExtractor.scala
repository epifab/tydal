package io.epifab.yadl.typesafe.runner

import java.sql.ResultSet

import io.epifab.yadl.typesafe.Tag
import io.epifab.yadl.typesafe.fields._
import shapeless.{::, HList, HNil}

trait FieldsExtractor[-RESULTS, FIELDS, TYPES] {
  def extract(fields: FIELDS, resultSet: RESULTS): Either[DecoderError, TYPES]
}

object FieldsExtractor {
  implicit def jdbcField[T]: FieldsExtractor[ResultSet, Field[T] with Tag[_], T] =
    new FieldsExtractor[ResultSet, Field[T] with Tag[_], T] {
      def getSeq[U](dbType: FieldType[U], fieldName: String, resultSet: ResultSet): Seq[U] =
        resultSet
          .getArray(fieldName)
          .asInstanceOf[Array[U]]
          .toSeq

      def get[U](dbType: FieldType[U], fieldName: String, resultSet: ResultSet): U = {
        dbType match {
          case TypeString | TypeDate | TypeDateTime | TypeJson | TypeEnum(_) | TypeGeography | TypeGeometry =>
            resultSet.getObject(fieldName).toString

          case TypeInt =>
            resultSet.getInt(fieldName)

          case TypeDouble =>
            resultSet.getDouble(fieldName)

          case TypeSeq(innerType) =>
            getSeq(innerType, fieldName, resultSet)

          case TypeOption(innerDbType) =>
            Option(resultSet.getObject(fieldName))
              .map(_ => get(innerDbType, fieldName, resultSet))
        }
      }

      override def extract(fields: Field[T] with Tag[_], resultSet: ResultSet): Either[DecoderError, T] = {
        val decoder = fields.decoder
        decoder.decode(get(decoder.dbType, fields.tagValue, resultSet))
      }
    }

  implicit def hNil[RESULTS]: FieldsExtractor[RESULTS, HNil, HNil] =
    (_: HNil, _: RESULTS) => Right(HNil)

  implicit def hCons[RESULTS, H, HX, T <: HList, TX <: HList](implicit head: FieldsExtractor[RESULTS, H, HX], tail: FieldsExtractor[RESULTS, T, TX]): FieldsExtractor[RESULTS, H :: T, HX :: TX] =
    (fields: H :: T, resultSet: RESULTS) => for {
      h <- head.extract(fields.head, resultSet)
      t <- tail.extract(fields.tail, resultSet)
    } yield h :: t
}
