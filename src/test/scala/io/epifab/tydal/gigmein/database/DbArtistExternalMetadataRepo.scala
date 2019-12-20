package io.epifab.tydal.gigmein.database

import io.epifab.tydal._
import io.epifab.tydal.gigmein.database.Schema._
import io.epifab.tydal.gigmein.domain.model.{ArtistBasicInfo, ArtistExternalMetadata}
import io.epifab.tydal.gigmein.domain.repos.ArtistExternalMetadataRepo
import io.epifab.tydal.queries.{Insert, Select}
import io.epifab.tydal.runtime.{ReadStatement, Transaction}
import io.epifab.tydal.schema.TableBuilder
import shapeless.{::, HNil}

abstract class DbArtistExternalMetadataRepo[TableName <: String with Singleton](tableName: TableName)(implicit valueOf: ValueOf[TableName]) extends ArtistExternalMetadataRepo[Transaction] {
  object ArtistsMetadata extends TableBuilder[TableName, ArtistMetadataFields]

  override def findArtistsWithoutMetadata(limit: Int): Transaction[Seq[ArtistBasicInfo]] =
    artistsWithoutMetadataQuery
      .run((
        "offset" ~~> 0L,
        "limit" ~~> limit
      ))

  override def addArtistMetadata(artistData: ArtistExternalMetadata): Transaction[Unit] =
    Insert.into(ArtistsMetadata).compile
      .run((
        "artist_id" ~~> artistData.artistId,
        "external_id" ~~> artistData.externalId,
        "tags" ~~> artistData.tags,
        "picture_url" ~~> artistData.pictureUrl,
      ))
      .discardResults

  private val artistsWithoutMetadataQuery: ReadStatement[("offset" ~~> Long) :: ("limit" ~~> Int) :: HNil, ArtistBasicInfo, Vector] =
    Select
      .from(Artists as "a")
      .leftJoin(ArtistsMetadata as "am")
      .on(_ ("artist_id") === _ ("a", "id"))
      .take($ => ($("a", "id"), $("a", "name")))
      .where($ => $("am", "artist_id").isNotDefined)
      .inRange["offset", "limit"]
      .compile
      .rawTo { case id :: name :: HNil => ArtistBasicInfo(id, name) }
      .as[Vector]
}

object DbArtistSpotifyMetadataRepo extends DbArtistExternalMetadataRepo("spotify_artists")

object DbArtistLastFmMetadataRepo extends DbArtistExternalMetadataRepo("lastfm_artists")
