package io.epifab.tydal.gigmein.fixtures

import java.util.UUID

import io.epifab.tydal.gigmein.domain.model._

trait Artists {
  val radiohead = Artist(
    id = UUID.randomUUID,
    name = "test-Radiohead",
    tags = Seq("indie", "psychedelic", "rock"),
    imageUrl = Some("http://gigme.in/radiohead.jpg"),
    spotifyId = Some("4Z8W4fKeB5YxbusRsdQVPb"),
    lastFmId = Some("a74b1b7f-71a5-4011-9441-d0b5e4122711")
  )

  val massiveAttack = Artist(
    id = UUID.randomUUID,
    name = "test-Massive Attack",
    tags = Seq("trip-hop"),
    imageUrl = Some("http://gigme.in/massive-attack.jpg"),
    spotifyId = Some("6FXMGgJwohJLUSr5nVlf9X"),
    lastFmId = Some("10adbe5e-a2c0-4bf3-8249-2b4cbf6e6ca8")
  )

  val caribou = Artist(
    id = UUID.randomUUID,
    name = "test-Caribou",
    tags = Seq("indie"),
    imageUrl = Some("http://gigme.in/caribou.jpg"),
    spotifyId = None,
    lastFmId = Some("735e3514-a8ae-401f-af3b-6300df1b8d2c")
  )

  val explosionsInTheSky = Artist(
    id = UUID.randomUUID,
    name = "test-Explosions in the Sky",
    tags = Seq("post-rock"),
    imageUrl = Some("http://gigme.in/explosions-in-the-sky.jpg"),
    spotifyId = Some("1uQWmt1OhuHGRKmZ2ZcL6p"),
    lastFmId = None
  )

  val nicolasJaar = Artist(
    id = UUID.randomUUID,
    name = "test-Nicolas Jaar",
    tags = Seq("electro", "psychedelic"),
    imageUrl = None,
    spotifyId = Some("5a0etAzO5V26gvlbmHzT9W"),
    lastFmId = None
  )

  val alienSun = Artist(
    id = UUID.randomUUID,
    name = "test-Alien Sun",
    tags = Seq.empty,
    imageUrl = None,
    spotifyId = None,
    lastFmId = None
  )

  val jonHopkins = Artist(
    id = UUID.randomUUID,
    name = "test-Jon Hopkins",
    tags = Seq("ambient", "electro"),
    imageUrl = Some("http://gigme.in/jon-hopkins.jpg"),
    spotifyId = Some("7yxi31szvlbwvKq9dYOmFI"),
    lastFmId = None
  )

  val stoneRoses = Artist(
    id = UUID.randomUUID,
    name = "test-Stone Roses",
    tags = Seq("madchester"),
    imageUrl = Some("http://gigme.in/stone-roses.jpg"),
    spotifyId = Some("1lYT0A0LV5DUfxr6doRP3d"),
    lastFmId = None
  )

  val happyMondays = Artist(
    id = UUID.randomUUID,
    name = "test-Happy Mondays",
    tags = Seq("madchester"),
    imageUrl = Some("http://gigme.in/happy-mondays.jpg"),
    spotifyId = Some("339DNkQkuhHKEcHw6oK8f0"),
    lastFmId = None
  )

  val candyFlip = Artist(
    id = UUID.randomUUID,
    name = "test-Candy Flip",
    tags = Seq("madchester"),
    imageUrl = Some("http://gigme.in/candy-flip.jpg"),
    spotifyId = None,
    lastFmId = None
  )

  val testArtists: List[Artist] = List(radiohead, massiveAttack, caribou, explosionsInTheSky, nicolasJaar)
}
