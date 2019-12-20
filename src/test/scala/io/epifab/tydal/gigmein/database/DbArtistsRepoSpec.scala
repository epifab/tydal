package io.epifab.tydal.gigmein.database

import java.util.UUID

import io.epifab.tydal.FunctionalTestBase
import io.epifab.tydal.gigmein.database.Schema.{ArtistLookups, Artists}
import io.epifab.tydal.gigmein.domain.model.ArtistBasicInfo
import io.epifab.tydal.queries.Delete
import io.epifab.tydal.runtime.{PostgresConfig, PostgresConnection}
import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

class DbArtistsRepoSpec extends FlatSpec with FunctionalTestBase with BeforeAndAfterEach with Matchers {

  private val artist = ArtistBasicInfo(UUID.randomUUID, "Test!!")

  override def afterEach(): Unit = {
    (for {
      _ <- Delete.from(ArtistLookups).compile.run(())
      _ <- Delete.from(Artists).compile.run(())
    } yield ()).runSync()
  }

  it should "create, find, update and delete an artist" in {
    val results = (for {
      _ <- DbArtistsRepo.addArtist(None, artist)
      created <- DbArtistsRepo.findArtistById(artist.id)
      _ <- DbArtistsRepo.removeArtist(artist.id)
      deleted <- DbArtistsRepo.findArtistById(artist.id)
    } yield (created, deleted)).runSync()

    results shouldBe Right(Some(artist), None)
  }

  it should "import and find an artist" in {
    val lookupKey = UUID.randomUUID

    val results = (for {
      _ <- DbArtistsRepo.addArtist(Some(lookupKey), artist)
      created <- DbArtistsRepo.lookupArtist(lookupKey)
    } yield created).runSync()

    results shouldBe Right(Some(artist))
  }
}
