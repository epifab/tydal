package io.epifab.yadl.typesafe.runner

import java.sql.ResultSet

import io.epifab.yadl.typesafe.{Query, Tag}
import io.epifab.yadl.typesafe.fields._
import shapeless.{::, Generic, HList, HNil}

/**
 * Type class to extract data from a result set
 *
 * @tparam RS Result set
 * @tparam FIELDS Fields definitions
 * @tparam OUTPUT Data output
 */
trait DataExtractor[RS, FIELDS, OUTPUT] {
  def extract(resultSet: RS, fields: FIELDS): Either[DecoderError, OUTPUT]
}

object DataExtractor {
  class Extractor[RESULTS, FIELDS](resultSet: RESULTS, fields: FIELDS) {
    def as[OUTPUT](implicit dataExtractor: DataExtractor[RESULTS, FIELDS, OUTPUT]): Either[DecoderError, OUTPUT] =
      dataExtractor.extract(resultSet, fields)
  }

  def apply[RESULTS, FIELDS](resultSet: RESULTS, fields: FIELDS): Extractor[RESULTS, FIELDS] =
    new Extractor(resultSet, fields)

  implicit def jdbcField[F <: Field[_] with Tag[_], T](implicit fieldT: FieldT[F, T]): DataExtractor[ResultSet, F, T] =
    new DataExtractor[ResultSet, F, T] {
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

      override def extract(resultSet: ResultSet, field: F): Either[DecoderError, T] = {
        val decoder: FieldDecoder[T] = fieldT.get(field).decoder
        decoder.decode(get(decoder.dbType, field.tagValue, resultSet))
      }
    }

  implicit def hNil[RS]: DataExtractor[RS, HNil, HNil] =
    (_: RS, _: HNil) => Right(HNil)

  implicit def hCons[RS, H, HX, T <: HList, TX <: HList]
      (implicit
       head: DataExtractor[RS, H, HX],
       tail: DataExtractor[RS, T, TX]): DataExtractor[RS, H :: T, HX :: TX] =
    (resultSet: RS, fields: H :: T) => for {
      h <- head.extract(resultSet, fields.head)
      t <- tail.extract(resultSet, fields.tail)
    } yield h :: t

  implicit def caseClass[RS, FIELDS, CC, REPR]
      (implicit
       generic: Generic.Aux[CC, REPR],
       dataExtractor: DataExtractor[RS, FIELDS, REPR]): DataExtractor[RS, FIELDS, CC] =
    (resultSet: RS, fields: FIELDS) => dataExtractor.extract(resultSet, fields).map(generic.from)
}
