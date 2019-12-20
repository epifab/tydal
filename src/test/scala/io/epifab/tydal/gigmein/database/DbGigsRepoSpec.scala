package io.epifab.tydal.gigmein.database

import java.time.{Instant, LocalDate}
import java.util.UUID

import cats.effect.{ContextShift, IO}
import io.epifab.tydal.FunctionalTestBase
import io.epifab.tydal.gigmein.database.Schema.{ArtistLookups, Artists, Gigs, GigsArtists, VenueLookups, Venues}
import io.epifab.tydal.gigmein.domain.model.{ArtistExternalMetadata, Gig, GigSource, Venue}
import io.epifab.tydal.gigmein.domain.repos.GigsRepo.{Distance, SearchCriteria}
import io.epifab.tydal.gigmein.fixtures.TestData
import io.epifab.tydal.queries.Delete
import io.epifab.tydal.runtime.{DataError, PostgresConfig, PostgresConnection, Transaction}
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, Matchers}

class DbGigsRepoSpec extends FlatSpec with Matchers with FunctionalTestBase with BeforeAndAfterAll with BeforeAndAfterEach with TestData {

  override def beforeAll(): Unit = {
    val artistsTransactions =
      testArtists.map(DbArtistsRepo.addArtist(None, _))

    val spotifyMetadata =
      testArtists.flatMap(a => a.spotifyId.map(id =>
        DbArtistSpotifyMetadataRepo
          .addArtistMetadata(ArtistExternalMetadata(
            a.id,
            Some(id),
            Some(a.tags),
            a.imageUrl)
          )
      ))

    val lastFmMetadata =
      testArtists.flatMap(a => a.lastFmId.map(id =>
        DbArtistLastFmMetadataRepo
          .addArtistMetadata(ArtistExternalMetadata(
            a.id,
            Some(id),
            Some(a.tags),
            a.imageUrl)
          )
      ))

    (for {
      _ <- Transaction.list(artistsTransactions)
      _ <- Transaction.parList(spotifyMetadata ++ lastFmMetadata)
      _ <- Transaction.parList(testVenues.map(DbVenuesRepo.addVenue(None, _)))
    } yield ()).runSync()
  }

  override def afterAll(): Unit = {
    (for {
      _ <- Transaction.parList(List(
             Delete.from(VenueLookups).compile,
             Delete.from(ArtistLookups).compile
           ).map(_.run(())))
      _ <- Transaction.parList(List(
             Delete.from(Venues).compile,
             Delete.from(Artists).compile
           ).map(_.run(())))
    } yield ()).runSync()
  }

  override def afterEach(): Unit = {
    (for {
      _ <- Delete.from(GigsArtists).compile.run(())
      _ <- Delete.from(Gigs).compile.run(())
    } yield ()).runSync()
  }

  private val addGig1: Transaction[Unit] = DbGigsRepo.addGig(testGig1)

  it should "create, find and delete a gig" in {
    val results = (for {
      _ <- addGig1
      maybeCreated <- DbGigsRepo.findGigByExternalLink(testGig1.externalLink.get)
      _ <- DbGigsRepo.removeGig(testGig1.id)
      maybeDeleted <- DbGigsRepo.findGigByExternalLink(testGig1.externalLink.get)
    } yield (maybeCreated, maybeDeleted)).runSync()

    results shouldBe Right((Some(testGig1), None))
  }

  it should "find gigs by start date" in {
    val findGigsInDate = DbGigsRepo.findGigs(
      user = None,
      SearchCriteria(
        location = Some(london),
        startDate = Some(testGig1.date)
      ))

    val findGigsOutDate = DbGigsRepo.findGigs(
      user = None,
      SearchCriteria(
        location = Some(london),
        startDate = Some(testGig1.date.plusDays(1))
      ))

    val results = (for {
      _ <- addGig1
      gigs1 <- findGigsInDate
      gigs2 <- findGigsOutDate
    } yield (gigs1, gigs2)).runSync()

     results shouldBe Right((Seq(testGig1), Seq.empty))
  }

  it should "find gigs by start/end date" in {
    val findGigsInDate = DbGigsRepo.findGigs(
      user = None,
      SearchCriteria(
        location = Some(london),
        startDate = Some(testGig1.date.minusDays(1)),
        endDate = Some(testGig1.date.plusDays(1))
      ))

    val findGigsOutDate = DbGigsRepo.findGigs(
      user = None,
      SearchCriteria(
        location = Some(london),
        startDate = Some(testGig1.date.minusDays(1)),
        endDate = Some(testGig1.date)
      ))

    val results = (for {
      _ <- addGig1
      gigs1 <- findGigsInDate
      gigs2 <- findGigsOutDate
    } yield (gigs1, gigs2)).runSync()

    results shouldBe Right((Seq(testGig1), Seq.empty))
  }

  it should "find gigs nearby" in {
    val findGigs = DbGigsRepo.findGigs(
      user = None,
      SearchCriteria(
        startDate = Some(testGig1.date),
        maxDistance = camdenHead.coordinates.map(_ -> Distance.meters(5000))
      )
    )

    val results = (for {
      _ <- addGig1
      gigs <- findGigs
    } yield gigs).runSync()

    val venue: Either[DataError, Option[Venue]] = results.map(_.headOption.map(_.venue))

    venue.map(_.map(_.id)) shouldBe Right(Some(theOldQueensHead.id))
    venue.map(_.flatMap(_.distance)) shouldBe Right(Some(4497.38772308))
  }

  it should "find gigs by query when matching an artist name" in {
    val findGigs = DbGigsRepo.findGigs(user = None, SearchCriteria(
      location = Some(london),
      startDate = Some(testGig1.date),
      query = Some("Radio")
    ))

    val results = (for {
      _ <- addGig1
      gigs <- findGigs
    } yield gigs).runSync()

    results shouldBe Right(Seq(testGig1))
  }

  it should "find gigs by query when matching the venue name" in {
    val findGigs = DbGigsRepo.findGigs(user = None, SearchCriteria(
      location = Some(london),
      startDate = Some(testGig1.date),
      query = Some("Queens")
    ))

    val results = (for {
      _ <- addGig1
      gigs <- findGigs
    } yield gigs).runSync()

    results shouldBe Right(Seq(testGig1))
  }

  it should "not find gigs by query when not matching anything" in {
    val findGigs = DbGigsRepo.findGigs(user = None, SearchCriteria(
      location = Some(london),
      startDate = Some(testGig1.date),
      query = Some("roar!")
    ))

    val results = (for {
      _ <- addGig1
      gigs <- findGigs
    } yield gigs).runSync()

    results shouldBe Right(Seq.empty)
  }

  it should "find gigs by tags" in {
    val findGigs1 = DbGigsRepo.findGigs(user = None, SearchCriteria(
      location = Some(london),
      startDate = Some(testGig1.date),
      tags = Seq("trip-hop")
    ))

    val findGigs2 = DbGigsRepo.findGigs(user = None, SearchCriteria(
      location = Some(london),
      startDate = Some(testGig1.date),
      tags = Seq("trip-hop", "samba"),
    ))

    val findGigs3 = DbGigsRepo.findGigs(user = None, SearchCriteria(
      location = Some(london),
      startDate = Some(testGig1.date),
      tags = Seq("samba")
    ))

    val results = (for {
      _ <- addGig1
      gigs1 <- findGigs1
      gigs2 <- findGigs2
      gigs3 <- findGigs3
    } yield (gigs1, gigs2, gigs3)).runSync()

    results shouldBe Right(Seq(testGig1), Seq(testGig1), Seq.empty)
  }

  it should "count gigs by start date" in {
    val count1 = DbGigsRepo.countGigs(user = None, SearchCriteria(
      location = Some(london),
      startDate = Some(testGig1.date),
    ))

    val find1 = DbGigsRepo.findGigs(user = None, SearchCriteria(
      location = Some(london),
      startDate = Some(testGig1.date),
    ))

    val count2 = DbGigsRepo.countGigs(user = None, SearchCriteria(
      location = Some(london),
      startDate = Some(testGig1.date.plusDays(1)),
    ))

    val count = (for {
      _ <- addGig1
      c1 <- count1
      c2 <- count2
    } yield (c1, c2)).runSync()

    count shouldBe Right((1, 0))
  }

  it should "count gigs by start/end date" in {
    val count1 = DbGigsRepo.countGigs(user = None, SearchCriteria(
      location = Some(london),
      startDate = Some(testGig1.date),
      endDate = Some(testGig1.date.plusDays(1)),
    ))

    val count2 = DbGigsRepo.countGigs(user = None, SearchCriteria(
      location = Some(london),
      startDate = Some(testGig1.date),
      endDate = Some(testGig1.date),
    ))

    val count = (for {
      _ <- addGig1
      c1 <- count1
      c2 <- count2
    } yield (c1, c2)).runSync()

    count shouldBe Right((1, 0))
  }

  it should "count gigs by query when matching an artist name" in {
    val add = addGig1

    val count = DbGigsRepo.countGigs(user = None, SearchCriteria(
      location = Some(london),
      startDate = Some(testGig1.date),
      query = Some("Radio"),
    ))

    val results = (for {
      _ <- add
      c <- count
    } yield c).runSync()

    results shouldBe Right(1)
  }

  it should "count gigs by query when matching the venue name" in {
    val count = DbGigsRepo.countGigs(user = None, SearchCriteria(
      location = Some(london),
      startDate = Some(testGig1.date),
      query = Some("Queens")
    ))

    val results = (for {
      _ <- addGig1
      c <- count
    } yield c).runSync()

    results shouldBe Right(1)
  }

  it should "count gigs by query when not matching anything" in {
    val count = DbGigsRepo.countGigs(user = None, SearchCriteria(
      location = Some(london),
      startDate = Some(testGig1.date),
      query = Some("roar!")
    ))

    val results = (for {
      _ <- addGig1
      c <- count
    } yield c).runSync()

    results shouldBe Right(0)
  }

  it should "count gigs by tags" in {
    val count1 = DbGigsRepo.countGigs(user = None, SearchCriteria(
      location = Some(london),
      startDate = Some(testGig1.date),
      tags = Seq("trip-hop")
    ))

    val count2 = DbGigsRepo.countGigs(user = None, SearchCriteria(
      location = Some(london),
      startDate = Some(testGig1.date),
      tags = Seq("trip-hop", "samba")
    ))

    val count3 = DbGigsRepo.countGigs(user = None, SearchCriteria(
      location = Some(london),
      startDate = Some(testGig1.date),
      tags = Seq("samba")
    ))

    val results = (for {
      _ <- addGig1
      c1 <- count1
      c2 <- count2
      c3 <- count3
    } yield (c1, c2, c3)).runSync()

    results shouldBe Right(1, 1, 0)
  }

  it should "paginate the results" in {
    implicit val ord: Ordering[LocalDate] =
      (x: LocalDate, y: LocalDate) => if (x.isEqual(y)) 0 else if (x.isBefore(y)) -1 else 1

    val find1 = DbGigsRepo.findGigs(user = None, SearchCriteria(
      location = Some(london),
      startDate = Some(testGigs.map(_.date).min)
    )).map(_.length)

    val find2 = DbGigsRepo.findGigs(user = None, SearchCriteria(
      location = Some(london),
      startDate = Some(testGigs.map(_.date).min),
      pageSize = 2
    )).map(_.length)

    val find3 = DbGigsRepo.findGigs(user = None, SearchCriteria(
      location = Some(london),
      startDate = Some(testGigs.map(_.date).min),
      pageOffset = 1,
      pageSize = 2
    )).map(_.length)

    val results = (for {
      _ <- addGig1
      _ <- DbGigsRepo.addGig(testGig2)
      _ <- DbGigsRepo.addGig(testGig3)
      f1 <- find1
      f2 <- find2
      f3 <- find3
    } yield (f1, f2, f3)).runSync()

    results shouldBe Right((3, 2, 1))
  }

  it should "implement entitlements filters" in {
    val gig = Gig(
      UUID.randomUUID,
      LocalDate.now.plusDays(10),
      Seq(radiohead),
      theOldQueensHead,
      None,
      GigSource.GigMeIn,
      Some(anAuthor),
      Instant.now,
      None,
      None,
      None
    )

    val results = (for {
      _ <- DbUsersRepo.addUser(anAuthor)
      _ <- DbUsersRepo.ensureRoles(anAuthor)
      _ <- DbUsersRepo.addUser(anotherAuthor)
      _ <- DbUsersRepo.ensureRoles(anotherAuthor)
      _ <- DbUsersRepo.addUser(aPublisher)
      _ <- DbUsersRepo.ensureRoles(aPublisher)
      _ <- DbGigsRepo.addGig(gig)

      a <- DbGigsRepo.findGigById(None, gig.id)
      a2 <- DbGigsRepo.findGigs(None, SearchCriteria())
      b <- DbGigsRepo.findGigById(Some(anAuthor), gig.id)
      b2 <- DbGigsRepo.findGigs(Some(anAuthor), SearchCriteria())
      c <- DbGigsRepo.findGigById(Some(anotherAuthor), gig.id)
      c2 <- DbGigsRepo.findGigs(Some(anotherAuthor), SearchCriteria())
      d <- DbGigsRepo.findGigById(Some(aPublisher), gig.id)
      d2 <- DbGigsRepo.findGigs(Some(aPublisher), SearchCriteria())
    } yield (a, b, c, d) -> (a2, b2, c2, d2)).runSync()

    results shouldBe Right((None, Some(gig), None, Some(gig)) -> (Seq.empty, Seq(gig), Seq.empty, Seq(gig)))
  }
}
