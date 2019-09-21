package io.epifab.yadl.typesafe.runner

import java.sql.ResultSet

import io.epifab.yadl.typesafe.Tag
import io.epifab.yadl.typesafe.fields._
import shapeless.{::, HList, HNil}

/**
 * Type class to extract data from a result set
 *
 * @tparam RESULTS Result set
 * @tparam FIELDS Fields definitions
 * @tparam OUTPUT Data output
 */
trait DataExtractor[RESULTS, FIELDS, OUTPUT] {
  def extract(resultSet: RESULTS, fields: FIELDS): Either[DecoderError, OUTPUT]
}

object DataExtractor {
  class Extractor[RESULTS, FIELDS](resultSet: RESULTS, fields: FIELDS) {
    def as[OUTPUT](implicit dataExtractor: DataExtractor[RESULTS, FIELDS, OUTPUT]): Either[DecoderError, OUTPUT] =
      dataExtractor.extract(resultSet, fields)
  }

  def apply[RESULTS, FIELDS](resultSet: RESULTS, fields: FIELDS): Extractor[RESULTS, FIELDS] =
    new Extractor(resultSet, fields)

  implicit def jdbcField[T, FIELD <: Field[T] with Tag[_]]: DataExtractor[ResultSet, FIELD, T] =
    new DataExtractor[ResultSet, FIELD, T] {
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

      override def extract(resultSet: ResultSet, fields: FIELD): Either[DecoderError, T] = {
        val decoder = fields.decoder
        decoder.decode(get(decoder.dbType, fields.tagValue, resultSet))
      }
    }

  implicit def hNil[RESULTS]: DataExtractor[RESULTS, HNil, HNil] =
    (_: RESULTS, _: HNil) => Right(HNil)

  implicit def hCons[RESULTS, H, HX, T <: HList, TX <: HList](implicit head: DataExtractor[RESULTS, H, HX], tail: DataExtractor[RESULTS, T, TX]): DataExtractor[RESULTS, H :: T, HX :: TX] =
    (resultSet: RESULTS, fields: H :: T) => for {
      h <- head.extract(resultSet, fields.head)
      t <- tail.extract(resultSet, fields.tail)
    } yield h :: t
}
