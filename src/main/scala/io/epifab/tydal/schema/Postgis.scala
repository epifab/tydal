package io.epifab.tydal.schema

import io.epifab.tydal.runtime.DecoderError

object Postgis {
  case class Geography(value: String)
  case class Geometry(value: String)

  case object TypeGeography extends FieldType[String]("geography")
  case object TypeGeometry extends FieldType[String]("geometry")

  trait IsSpatial[T] extends TypeProps

  object IsSpatial {
    implicit val geometry: IsSpatial[Geometry] = new IsSpatial[Geometry] {}
    implicit val geography: IsSpatial[Geography] = new IsSpatial[Geography] {}
    implicit def optional[T](implicit isSpatial: IsSpatial[T]): IsSpatial[Option[T]] = new IsSpatial[Option[T]] {}

    implicit def field[F <: Field[_], T](implicit ft: FieldT[F, T], isSpatial: IsSpatial[T]): IsSpatial[F] = new IsSpatial[F] {}
  }

  object Distance {
    final private class Distance[+F1 <: Field[_], +F2 <: Field[_], +U] extends DbFunction2[F1, F2, U]("ST_Distance")

    def apply[F1 <: Field[_], F2 <: Field[_]](field1: F1, field2: F2)(
      implicit
      isSpatial1: IsSpatial[F1],
      isSpatial2: IsSpatial[F2],
      bothNonOptional: Negative[AnyOptional[F1, F2]],
      decoder: FieldDecoder[Double]
    ): FieldExpr2[F1, F2, Double] =
      FieldExpr2(field1, field2, new Distance)

    def optional[F1 <: Field[_], F2 <: Field[_]](field1: F1, field2: F2)(
      implicit
      isSpatial1: IsSpatial[F1],
      isSpatial2: IsSpatial[F2],
      anyOptional: AnyOptional[F1, F2],
      decoder: FieldDecoder[Option[Double]]
    ): FieldExpr2[F1, F2, Option[Double]] =
      FieldExpr2(field1, field2, new Distance)
  }

  object MakePoint {
    final private class MakePoint[+F1 <: Field[_], +F2 <: Field[_], +U] extends DbFunction2[F1, F2, U]("ST_MakePoint")

    def apply[F1 <: Field[_], F2 <: Field[_]](field1: F1, field2: F2)(
      implicit
      isDouble1: IsDouble[F1],
      isDouble2: IsDouble[F2],
      nonOption1: Negative[IsOptional[F1]],
      nonOption2: Negative[IsOptional[F2]],
      decoder: FieldDecoder[Geometry]
    ): FieldExpr2[F1, F2, Geometry] =
      FieldExpr2(field1, field2, new MakePoint)

    def optional[F1 <: Field[_], F2 <: Field[_]](field1: F1, field2: F2)(
      implicit
      isDouble1: IsDouble[F1],
      isDouble2: IsDouble[F2],
      anyOptional: AnyOptional[F1, F2],
      decoder: FieldDecoder[Geometry]
    ): FieldExpr2[F1, F2, Option[Geometry]] =
      FieldExpr2(field1, field2, new MakePoint)
  }

  object Latitude {
    final private class Latitude[+F1 <: Field[_], +U] extends DbFunction1[F1, U]("ST_X")

    def apply[F <: Field[_]](field: F)(
      implicit
      isSpatial: IsSpatial[F],
      nonOptional: Negative[IsOptional[F]],
      decoder: FieldDecoder[Double]
    ): FieldExpr1[F, Double] =
      FieldExpr1(field, new Latitude)

    def optional[F <: Field[_]](field: F)(
      implicit
      isSpatial: IsSpatial[F],
      isOptional: IsOptional[F],
      decoder: FieldDecoder[Option[Double]]
    ): FieldExpr1[F, Option[Double]] =
      FieldExpr1(field, new Latitude)
  }

  object Longitude {
    final private class Longitude[+F1 <: Field[_], +U] extends DbFunction1[F1, U]("ST_Y")

    def apply[F <: Field[_]](field: F)(
      implicit
      isSpatial: IsSpatial[F],
      nonOptional: Negative[IsOptional[F]],
      decoder: FieldDecoder[Double]
    ): FieldExpr1[F, Double] =
      FieldExpr1(field, new Longitude)

    def optional[F <: Field[_]](field: F)(
      implicit
      isSpatial: IsSpatial[F],
      isOptional: IsOptional[F],
      decoder: FieldDecoder[Option[Double]]
    ): FieldExpr1[F, Option[Double]] =
      FieldExpr1(field, new Longitude)
  }

  implicit val geographyDecoder: FieldDecoder.Aux[Geography, String] =
    new FieldDecoder[Geography] {
      override type DbType = String
      override def dbType: FieldType[String] = TypeGeography
      override def decode(value: String): Either[DecoderError, Geography] = Right(Geography(value))
    }

  implicit val geographyEncoder: FieldEncoder.Aux[Geography, String] =
    new FieldEncoder[Geography] {
      override type DbType = String
      override def dbType: FieldType[String] = TypeGeography
      override def encode(value: Geography): String = value.value
    }

  implicit val geometryDecoder: FieldDecoder.Aux[Geometry, String] =
    new FieldDecoder[Geometry] {
      override type DbType = String
      override def dbType: FieldType[String] = TypeGeometry
      override def decode(value: String): Either[DecoderError, Geometry] = Right(Geometry(value))
    }

  implicit val geometryEncoder: FieldEncoder.Aux[Geometry, String] =
    new FieldEncoder[Geometry] {
      override type DbType = String
      override def dbType: FieldType[String] = TypeGeometry
      override def encode(value: Geometry): String = value.value
    }
}
