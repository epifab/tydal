package io.epifab.tydal.gigmein.fixtures

import java.time.{Instant, LocalDate}
import java.util.UUID

import io.epifab.tydal.gigmein.domain.model.{Gig, GigSource}

trait TestData extends Venues with Artists with Users {
  private val firstOfJan: LocalDate = LocalDate.of(1999, 1, 1)
  private val secondOfJan: LocalDate = LocalDate.of(1999, 1, 2)
  private val thirdOfJan: LocalDate = LocalDate.of(1999, 1, 3)
  private val fourthOfJan: LocalDate = LocalDate.of(1999, 1, 4)

  val testGig1 = Gig(
    UUID.randomUUID,
    firstOfJan,
    Seq(radiohead, massiveAttack),
    theOldQueensHead,
    Some("http://gigme.in/test-gig-1"),
    source = GigSource.Songkick,
    authored = Instant.now,
    author = None,
    published = Some(Instant.now),
    publisher = None
  )
  val testGig2 = Gig(
    UUID.randomUUID,
    secondOfJan,
    Seq(radiohead, caribou),
    londonInn,
    Some("http://gigme.in/test-gig-2"),
    source = GigSource.Songkick,
    authored = Instant.now,
    author = None,
    published = Some(Instant.now),
    publisher = None
  )

  val testGig3 = Gig(
    UUID.randomUUID,
    secondOfJan,
    Seq(explosionsInTheSky),
    camdenHead,
    Some("http://gigme.in/test-gig-3"),
    source = GigSource.Songkick,
    authored = Instant.now,
    author = None,
    published = Some(Instant.now),
    publisher = None
  )

  val testGig4 = Gig(
    UUID.randomUUID,
    thirdOfJan,
    Seq(explosionsInTheSky),
    camdenHead,
    Some("http://gigme.in/test-gig-4"),
    source = GigSource.Songkick,
    authored = Instant.now,
    author = None,
    published = Some(Instant.now),
    publisher = None
  )

  val testGig5 = Gig(
    UUID.randomUUID,
    fourthOfJan,
    Seq(nicolasJaar),
    theOldQueensHead,
    Some("http://gigme.in/test-gig-5"),
    source = GigSource.Songkick,
    authored = Instant.now,
    author = None,
    published = Some(Instant.now),
    publisher = None
  )

  val testGigs: Seq[Gig] = Seq(testGig1, testGig2, testGig3, testGig4, testGig5)
}

object TestData extends TestData
