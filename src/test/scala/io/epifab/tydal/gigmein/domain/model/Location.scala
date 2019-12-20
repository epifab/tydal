package io.epifab.tydal.gigmein.domain.model

import java.util.NoSuchElementException


case class Location(name: String, label: String, coordinates: Coordinates) {
  override def toString: String = name
}

object Location {
  val london = Location("london", "London (UK)", Coordinates(51.5131, -0.1221))
  val bristol = Location("bristol", "Bristol (UK)", Coordinates(51.4545, 2.5879))
  val amsterdam = Location("amsterdam", "Amsterdam", Coordinates(52.379189, 4.899431))
  val berlin = Location("berlin", "Berlin", Coordinates(52.5200, 13.4050))
  val barcelona = Location("barcelona", "Barcelona", Coordinates(41.3851, 2.1734))
  val newYork = Location("newyork", "New York", Coordinates(40.7128, 740060))

  val cities: Seq[Location] = List(
    london,
    bristol,
    newYork,
    amsterdam,
    barcelona
    // deliberately ignoring other locations for now
  )

  def apply(name: String): Location = {
    cities.find(_.name == name) match {
      case Some(location) => location
      case None => throw new NoSuchElementException("Location not found")
    }
  }
}
