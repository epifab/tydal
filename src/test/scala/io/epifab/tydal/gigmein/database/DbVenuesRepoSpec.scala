package io.epifab.tydal.gigmein.database

import java.util.UUID

import io.epifab.tydal.FunctionalTestBase
import io.epifab.tydal.gigmein.database.Schema.{Gigs, GigsArtists, VenueLookups, Venues}
import io.epifab.tydal.gigmein.fixtures.TestData
import io.epifab.tydal.queries.Delete
import io.epifab.tydal.runtime.{PostgresConfig, PostgresConnection}
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, Matchers}

class DbVenuesRepoSpec extends FlatSpec with FunctionalTestBase with Matchers with BeforeAndAfterAll with BeforeAndAfterEach with TestData {

  override def afterEach(): Unit = {
    (for {
      _ <- Delete.from(VenueLookups).compile.run(())
      _ <- Delete.from(Venues).compile.run(())
    } yield ()).runSync()
  }

  it should "import a and find a venue" in {
    val key = UUID.randomUUID

    val results = (for {
      _ <- DbVenuesRepo.addVenue(Some(key), theOldQueensHead)
      v <- DbVenuesRepo.lookupVenue(key)
    } yield v).runSync()

    results shouldBe Right(Some(theOldQueensHead))
  }

  it should "find venues by name" in {
    val results = (for {
      _ <- DbVenuesRepo.addVenue(None, theOldQueensHead)
      _ <- DbVenuesRepo.addVenue(None, camdenHead)
      _ <- DbVenuesRepo.addVenue(None, londonInn)
      f1 <- DbVenuesRepo.findVenues(london, "head", 0, 2)
      f2 <- DbVenuesRepo.countVenues(london, "head")
      f3 <- DbVenuesRepo.findVenues(london, "head", pageOffset = 0, pageSize = 1)
      f4 <- DbVenuesRepo.findVenues(london, "head", pageOffset = 1, pageSize = 1)
      f5 <- DbVenuesRepo.findVenues(london, "head", pageOffset = 2, pageSize = 1)
    } yield (f1, f2, f3, f4, f5)).runSync()

    results shouldBe Right(Seq(camdenHead, theOldQueensHead), 2, Seq(camdenHead), Seq(theOldQueensHead), Seq.empty)
  }
}
