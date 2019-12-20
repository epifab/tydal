package io.epifab.tydal.gigmein.fixtures

import java.util.UUID

import io.epifab.tydal.gigmein.domain.model.{Coordinates, Location, Venue}

trait Venues {
  val london: Location = Location.london

  val theOldQueensHead = Venue(UUID.randomUUID, london.name, "Old Queens Head", Some("123 Fake St."), Some(Coordinates(51.5373, 0.100447)))
  val camdenHead = Venue(UUID.randomUUID, london.name, "Camden Head", Some("234 Fake St."), Some(Coordinates(51.53730, 0.14112)))
  val londonInn = Venue(UUID.randomUUID, london.name, "London Inn", None, None)

  val testVenues: List[Venue] = List(theOldQueensHead, camdenHead, londonInn)
}

object Venues extends Venues
