package io.epifab.tydal.gigmein.database

import io.epifab.tydal._
import io.epifab.tydal.gigmein.database.Schema.{VenueLookups, Venues}
import io.epifab.tydal.gigmein.domain.model.{Coordinates, Id, Location, Venue}
import io.epifab.tydal.gigmein.domain.repos.VenuesRepo
import io.epifab.tydal.queries.{Ascending, Insert, Select, Update}
import io.epifab.tydal.runtime.{ReadStatement, ResultSet, Transaction}
import io.epifab.tydal.schema.Postgis.{Geography, Geometry, Latitude, Longitude, MakePoint}
import io.epifab.tydal.schema.{Count, Literal, NamedPlaceholder}
import shapeless.{::, HNil}

object DbVenuesRepo extends VenuesRepo[Transaction] {
  override def lookupVenue(lookupKey: Id): Transaction[Option[Venue]] =
    lookupQuery.run(Tuple1("key" ~~> lookupKey))

  override def findVenues(location: Location, name: String, pageOffset: Long, pageSize: Int): Transaction[Seq[Venue]] =
    findVenuesByNameAndLocationQuery
      .run((
        "location" ~~> location.name,
        "name" ~~> s"%$name%",
        "offset" ~~> (pageOffset * pageSize),
        "limit" ~~> pageSize
      ))

  override def countVenues(location: Location, name: String): Transaction[Long] = {
    countVenuesQuery
      .run((
        "location" ~~> location.name,
        "name" ~~> s"%$name%"
      ))
  }

  override def findNonLocatedVenues(limit: Int): Transaction[Seq[Venue]] =
    nonLocatedVenuesQuery
      .run((
        "offset" ~~> 0L,
        "limit" ~~> limit
      ))

  private val findVenueBaseQuery =
    Select
      .from(Venues as "v")
      .focus("v").take(v => (
      v("id"),
      v("location"),
      v("name"),
      v("address"),
      Latitude(v("geo_location").castTo[Geometry]) as "lat",
      Longitude(v("geo_location").castTo[Geometry]) as "lng"
    ))

  private val nonLocatedVenuesQuery: ReadStatement[("offset" ~~> Long) :: ("limit" ~~> Int) :: HNil, Venue, Vector] =
    findVenueBaseQuery
      .where(_ ("v", "geo_location").isNotDefined)
      .inRange["offset", "limit"]
      .compile
      .to { resultSet => toVenue(resultSet) }
      .as[Vector]

  override def findVenueById(venueId: Id): Transaction[Option[Venue]] =
    findVenueByIdQuery
      .run(Tuple1("id" ~~> venueId))

  override def geoLocateVenue(venueId: Id, coordinates: Coordinates): Transaction[Unit] =
    updateVenueQuery
      .run((
        "lat" ~~> Some(coordinates.lat),
        "lng" ~~> Some(coordinates.lng),
        "id" ~~> venueId
      ))
      .discardResults

  override def addVenue(lookupKey: Option[Id], venue: Venue): Transaction[Unit] =
    for {
      _ <- insertVenue(venue)
      _ <- lookupKey.map(key => insertLookup(key, venue)).getOrElse(Transaction.successful(0))
   } yield ()

  private def insertVenue(venue: Venue) =
    insertVenueQuery
      .run((
        "id" ~~> venue.id,
        "location" ~~> venue.location,
        "name" ~~> venue.name,
        "address" ~~> venue.address,
        "lat" ~~> venue.coordinates.map(_.lat),
        "lng" ~~> venue.coordinates.map(_.lng)
      ))

  private def insertLookup(key: Id, venue: Venue) =
    insertLookupQuery.run((
      "id" ~~> venue.id,
      "lookup" ~~> key
    ))

  private val insertLookupQuery =
    Insert
      .into(VenueLookups)
      .compile

  private val insertVenueQuery =
    Insert
      .into(Venues)
      .set(
        MakePoint(
          NamedPlaceholder[Option[Double], "lat"],
          NamedPlaceholder[Option[Double], "lng"]
        ).castTo[Geography] as "geo_location"
      )
      .compile

  private val updateVenueQuery =
    Update(Venues)
      .fields($ => Tuple1($("geo_location")))
      .set(
        MakePoint(
          NamedPlaceholder[Option[Double], "lat"],
          NamedPlaceholder[Option[Double], "lng"]
        ).castTo[Geography] as "geo_location"
      )
      .where(_("id") === "id")
      .compile

  private val countVenuesQuery: ReadStatement[("location" ~~> String) :: ("name" ~~> String) :: HNil, Long, cats.Id] =
    findVenueBaseQuery
      .focus("v").where(v => (v("location") === "location") and (v("name") ilike "name"))
      .take1($ => Count($("v", "id")) as "num_venues")
      .compile
      .head
      .last(0)

  private val findVenueByIdQuery: ReadStatement[("id" ~~> Id) :: HNil, Venue, Option] =
    findVenueBaseQuery
      .where(_("v", "id") === "id")
      .compile
      .to(rs => toVenue(rs))
      .asOption

  private val lookupQuery: ReadStatement[("key" ~~> Id) :: HNil, Venue, Option] =
    findVenueBaseQuery
      .innerJoin(VenueLookups as "vl").on(_ ("id") === _ ("v", "id"))
      .where(_("vl", "lookup") === "key")
      .compile
      .to { resultSet => toVenue(resultSet) }
      .asOption

  private val findVenuesByNameAndLocationQuery: ReadStatement[("location" ~~> String) :: ("name" ~~> String) :: ("offset" ~~> Long) :: ("limit" ~~> Int) :: HNil, Venue, Vector] =
    findVenueBaseQuery
      .focus("v").where(v => (v("location") === "location") and (v("name") ilike "name"))
      .sortBy($ => Tuple1(Ascending($("v", "name"))))
      .inRange["offset", "limit"]
      .compile
      .to { resultSet => toVenue(resultSet) }
      .as[Vector]

  private def toVenue(resultSet: ResultSet[(Id As "id") :: (String As "location") :: (String As "name") :: (Option[String] As "address") :: (Option[Double] As "lat") :: (Option[Double] As "lng") :: HNil]) = {
    Venue(resultSet("id"), resultSet("location"), resultSet("name"), resultSet("address"), for {
      x <- resultSet("lat")
      y <- resultSet("lng")
    } yield Coordinates(x, y))
  }
}
