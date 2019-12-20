package io.epifab.tydal.gigmein.domain.model

trait ArtistBasicInfo {
  def id: Id
  def name: String

  override def equals(obj: Any): Boolean = obj match {
    case artist: ArtistBasicInfo =>
      artist.id == id && artist.name == name
    case _ => false
  }
}

object ArtistBasicInfo {
  case class ArtistBasicInfoImpl(id: Id, name: String) extends ArtistBasicInfo

  def apply(id: Id, name: String): ArtistBasicInfo =
    ArtistBasicInfoImpl(id, name)
}

trait ArtistMetadata {
  def imageUrl: Option[String]
  def tags: Seq[String]
}

trait ArtistExternalIds {
  def spotifyId: Option[String]
  def lastFmId: Option[String]
}

case class Artist(
  id:        Id,
  name:      String,
  tags:      Seq[String],
  imageUrl:  Option[String],
  spotifyId: Option[String],
  lastFmId:  Option[String]
) extends ArtistBasicInfo with ArtistMetadata with ArtistExternalIds

case class ArtistExternalMetadata(
  artistId: Id,
  externalId: Option[String],
  tags: Option[Seq[String]],
  pictureUrl: Option[String]
)
