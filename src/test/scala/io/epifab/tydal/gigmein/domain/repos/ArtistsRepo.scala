package io.epifab.tydal.gigmein.domain.repos

import io.epifab.tydal.gigmein.domain.model._

trait ArtistExternalMetadataRepo[F[_]] {
  def findArtistsWithoutMetadata(limit: Int): F[Seq[ArtistBasicInfo]]
  def addArtistMetadata(artistData: ArtistExternalMetadata): F[Unit]
}

trait ArtistsRepo[F[_]] {
  def lookupArtist(lookupKey: Id): F[Option[Artist]]
  def findArtistById(artistId: Id): F[Option[Artist]]
  def findArtistsByGigIds(gigIds: Seq[Id]): F[Map[Id, Seq[Artist]]]
  def findArtistsByName(name: String, pageOffset: Long, pageSize: Int): F[Seq[Artist]]
  def countArtistsByName(name: String): F[Long]

  def addArtist(lookupKey: Option[Id], artist: ArtistBasicInfo): F[Unit]
  def removeArtist(artistId: Id): F[Unit]
}
