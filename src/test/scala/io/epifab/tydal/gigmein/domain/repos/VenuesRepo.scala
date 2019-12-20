package io.epifab.tydal.gigmein.domain.repos

import io.epifab.tydal.gigmein.domain.model.{Coordinates, Id, Location, Venue}

trait VenuesRepo[F[_]] {
  def lookupVenue(lookupKey: Id): F[Option[Venue]]
  def findVenues(location: Location, name: String, pageOffset: Long, pageSize: Int): F[Seq[Venue]]
  def countVenues(location: Location, name: String): F[Long]
  def findNonLocatedVenues(limit: Int): F[Seq[Venue]]
  def findVenueById(venueId: Id): F[Option[Venue]]
  def geoLocateVenue(venueId: Id, coordinates: Coordinates): F[Unit]
  def addVenue(lookupKey: Option[Id], venue: Venue): F[Unit]
}
