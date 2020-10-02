package tydal.schema

import tydal.runtime.DecoderError

object Postgis {
  case class Geography(value: String)
  case class Geometry(value: String)

  case object TypeGeography extends FieldType[String]("geography")
  case object TypeGeometry extends FieldType[String]("geometry")

  trait IsSpatial[T] extends TypeProps

  object IsSpatial {
    implicit def isGeometry[T](implicit isGeometry: IsGeometry[T]): IsSpatial[T] = new IsSpatial[T] {}
    implicit def isGeography[T](implicit isGeography: IsGeography[T]): IsSpatial[T] = new IsSpatial[T] {}
  }

  trait IsGeometry[T] extends TypeProps

  object IsGeometry {
    implicit val geometry: IsGeometry[Geometry] = new IsGeometry[Geometry] {}
    implicit def optional[T](implicit isGeometry: IsGeometry[T]): IsGeometry[Option[T]] = new IsGeometry[Option[T]] {}

    implicit def field[F <: Field[_], T](implicit ft: FieldT[F, T], isGeometry: IsGeometry[T]): IsGeometry[F] = new IsGeometry[F] {}
  }

  trait IsGeography[T] extends TypeProps

  object IsGeography {
    implicit val geography: IsGeography[Geography] = new IsGeography[Geography] {}
    implicit def optional[T](implicit isGeography: IsGeography[T]): IsGeography[Option[T]] = new IsGeography[Option[T]] {}

    implicit def field[F <: Field[_], T](implicit ft: FieldT[F, T], isGeography: IsGeography[T]): IsGeography[F] = new IsGeography[F] {}
  }

  object Distance {
    final private class Distance[+F1 <: Field[_], +F2 <: Field[_], +U] extends DbFunction2[F1, F2, U]("ST_Distance")

    sealed trait ToDistance[F1 <: Field[_], F2 <: Field[_], P] {
      def apply(field1: F1, field2: F2): FieldExpr2[F1, F2, P]
    }

    object ToDistance {
      implicit def nonOptional[F1 <: Field[_], F2 <: Field[_]](
        implicit
        isGeography1: IsGeography[F1],
        isGeography2: IsGeography[F2],
        nonOptional1: Negative[IsOptional[F1]],
        nonOptional2: Negative[IsOptional[F2]]
      ): ToDistance[F1, F2, Double] = new ToDistance[F1, F2, Double] {
        override def apply(field1: F1, field2: F2): FieldExpr2[F1, F2, Double] =
          FieldExpr2(field1, field2, new Distance)
      }

      implicit def optional[F1 <: Field[_], F2 <: Field[_]](
        implicit
        IsGeography1: IsGeography[F1],
        IsGeography2: IsGeography[F2],
        anyOptional: AnyOptional[F1, F2]
      ): ToDistance[F1, F2, Option[Double]] = new ToDistance[F1, F2, Option[Double]] {
        override def apply(field1: F1, field2: F2): FieldExpr2[F1, F2, Option[Double]] =
          FieldExpr2(field1, field2, new Distance)
      }
    }

    def apply[F1 <: Field[_], F2 <: Field[_], R](field1: F1, field2: F2)(
      implicit toDistance: ToDistance[F1, F2, R]
    ): FieldExpr2[F1, F2, R] = toDistance(field1, field2)
  }

  object MakePoint {
    final private class MakePoint[+F1 <: Field[_], +F2 <: Field[_], +U] extends DbFunction2[F1, F2, U]("ST_MakePoint")

    sealed trait PointMaker[F1 <: Field[_], F2 <: Field[_], P] {
      def apply(field1: F1, field2: F2): FieldExpr2[F1, F2, P]
    }

    object PointMaker {
      implicit def nonOptional[F1 <: Field[_], F2 <: Field[_]](
        implicit
        isDouble1: IsDouble[F1],
        isDouble2: IsDouble[F2],
        nonOptional1: Negative[IsOptional[F1]],
        nonOptional2: Negative[IsOptional[F2]]
      ): PointMaker[F1, F2, Geometry] = new PointMaker[F1, F2, Geometry] {
        override def apply(field1: F1, field2: F2): FieldExpr2[F1, F2, Geometry] =
          FieldExpr2(field1, field2, new MakePoint)
      }

      implicit def optional[F1 <: Field[_], F2 <: Field[_]](
        implicit
        isDouble1: IsDouble[F1],
        isDouble2: IsDouble[F2],
        anyOptional: AnyOptional[F1, F2],
        decoder: FieldDecoder[Geometry]
      ): PointMaker[F1, F2, Option[Geometry]] = new PointMaker[F1, F2, Option[Geometry]] {
        override def apply(field1: F1, field2: F2): FieldExpr2[F1, F2, Option[Geometry]] =
          FieldExpr2(field1, field2, new MakePoint)
      }
    }

    def apply[F1 <: Field[_], F2 <: Field[_], R](field1: F1, field2: F2)(
      implicit pointMaker: PointMaker[F1, F2, R]
    ): FieldExpr2[F1, F2, R] = pointMaker(field1, field2)
  }

  object Latitude {
    final private class Latitude[+F1 <: Field[_], +U] extends DbFunction1[F1, U]("ST_X")

    sealed trait ToLatitude[F <: Field[_], R] {
      def apply(field: F): FieldExpr1[F, R]
    }

    object ToLatitude {
      implicit def geometry[F <: Field[_]](implicit isGeometry: IsGeometry[F], nonOptional: Negative[IsOptional[F]]): ToLatitude[F, Double] =
        new ToLatitude[F, Double] {
          override def apply(field: F): FieldExpr1[F, Double] = FieldExpr1(field, new Latitude)
        }

      implicit def optional[F <: Field[_]](implicit isGeometry: IsGeometry[F], isOptional: IsOptional[F]): ToLatitude[F, Option[Double]] = new ToLatitude[F, Option[Double]] {
        override def apply(field: F): FieldExpr1[F, Option[Double]] = FieldExpr1(field, new Latitude)
      }
    }

    def apply[F <: Field[_], R](field: F)(
      implicit
      toLatitude: ToLatitude[F, R]
    ): FieldExpr1[F, R] = toLatitude(field)
  }

  object Longitude {
    final private class Longitude[+F1 <: Field[_], +U] extends DbFunction1[F1, U]("ST_Y")

    sealed trait ToLongitude[F <: Field[_], R] {
      def apply(field: F): FieldExpr1[F, R]
    }

    object ToLongitude {
      implicit def geometry[F <: Field[_]](implicit isGeometry: IsGeometry[F], nonOptional: Negative[IsOptional[F]]): ToLongitude[F, Double] =
        new ToLongitude[F, Double] {
          override def apply(field: F): FieldExpr1[F, Double] = FieldExpr1(field, new Longitude)
        }

      implicit def optional[F <: Field[_]](implicit isGeometry: IsGeometry[F], optional: IsOptional[F]): ToLongitude[F, Option[Double]] =
        new ToLongitude[F, Option[Double]] {
          override def apply(field: F): FieldExpr1[F, Option[Double]] = FieldExpr1(field, new Longitude)
        }
    }

    def apply[F <: Field[_], R](field: F)(
      implicit
      toLongitude: ToLongitude[F, R]
    ): FieldExpr1[F, R] = toLongitude(field)
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
