package io.epifab.tydal.gigmein.domain.model

case class Coordinates(lat: Double, lng: Double)

object Coordinates {
  def optional(maybeLat: Option[Double], maybeLng: Option[Double]): Option[Coordinates] =
    for {
      lat <- maybeLat
      lng <- maybeLng
    } yield Coordinates(lat, lng)
}

case class Venue(
  id: Id,
  location: String,
  name: String,
  address: Option[String],
  coordinates: Option[Coordinates],
  distance: Option[Double] = None
)
