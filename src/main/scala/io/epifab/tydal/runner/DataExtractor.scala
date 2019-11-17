package io.epifab.tydal.runner

import java.sql.ResultSet

import io.epifab.tydal.Tagging
import io.epifab.tydal.fields._
import shapeless.{::, HList, HNil}

import scala.util.Try

/**
 * Type class to extract data from a generic result set
 *
 * @tparam ResultSet Result set
 * @tparam Fields Fields definitions
 * @tparam Output Data output
 */
trait DataExtractor[ResultSet, Fields, Output] {
  def extract(resultSet: ResultSet, fields: Fields): Either[DecoderError, Output]
}

object DataExtractor {
  class Extractor[RS, Fields](resultSet: RS, fields: Fields) {
    def as[Output](implicit dataExtractor: DataExtractor[RS, Fields, Output]): Either[DecoderError, Output] =
      dataExtractor.extract(resultSet, fields)
  }

  def apply[RS, Fields](resultSet: RS, fields: Fields): Extractor[RS, Fields] =
    new Extractor(resultSet, fields)

  implicit def jdbcField[F <: Field[_] with Tagging[_], T](implicit fieldT: FieldT[F, T]): DataExtractor[ResultSet, F, T] =
    new DataExtractor[ResultSet, F, T] {
      def getSeq[U](resultSet: ResultSet, dbType: FieldType[Any], fieldName: String): Seq[Any] =
        resultSet
          .getArray(fieldName)
          .getArray
          .asInstanceOf[Array[U]]
          .toSeq

      def get[U](resultSet: ResultSet, dbType: FieldType[Any], fieldName: String): Any = {
        dbType match {
          case TypeString | TypeDate | TypeDateTime | TypeJson | TypeEnum(_) | TypeGeography | TypeGeometry =>
            resultSet.getObject(fieldName).toString

          case TypeInt =>
            resultSet.getInt(fieldName)

          case TypeDouble =>
            resultSet.getDouble(fieldName)

          case TypeSeq(innerType) =>
            getSeq(resultSet, innerType, fieldName)

          case TypeOption(innerDbType) =>
            Try(Option(resultSet.getObject(fieldName))).toOption.flatten
              .map(_ => get(resultSet, innerDbType, fieldName))
        }
      }

      override def extract(resultSet: ResultSet, field: F): Either[DecoderError, T] = {
        val decoder: FieldDecoder[T] = fieldT.get(field).decoder
        decoder.decode(get(resultSet, decoder.dbType, field.tagValue).asInstanceOf[decoder.DbType])
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
}
