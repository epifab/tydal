package io.epifab.tydal.gigmein.database

import java.time.{Instant, LocalDate}

import io.epifab.tydal.:=:
import io.epifab.tydal.gigmein.domain.model.{Id, Idp, Permission}
import io.epifab.tydal.runtime.DecoderError
import io.epifab.tydal.schema.Postgis.Geography
import io.epifab.tydal.schema.{FieldDecoder, FieldEncoder, TableBuilder}

object Schema {
  implicit val permissionEncoder: FieldEncoder[Permission] =
    FieldEncoder.stringEncoder.contramap(_.name)

  implicit val permissionDecoder: FieldDecoder[Permission] =
    FieldDecoder.stringDecoder.map(Permission.get(_).left.map(DecoderError))

  implicit val idpEncoder: FieldEncoder[Idp] =
    FieldEncoder.stringEncoder.contramap(_.value)

  implicit val idpDecoder: FieldDecoder[Idp] =
    FieldDecoder.stringDecoder.map(Idp.get(_).left.map(DecoderError))

  implicit val listSeq: FieldDecoder[Seq[String]] = FieldDecoder.stringDecoder.toSeq
  implicit val optListSeq: FieldDecoder[Option[Seq[String]]] = listSeq.toOption

  object Artists extends TableBuilder["artists", (
    "id" :=: Id,
    "name" :=: String
  )]

  object ArtistLookups extends TableBuilder["artist_lookups", (
    "id" :=: Id,
    "lookup" :=: Id
  )]

  type ArtistMetadataFields = (
    "artist_id" :=: Id,
    "external_id" :=: Option[String],
    "tags" :=: Option[Seq[String]],
    "picture_url" :=: Option[String]
  )

  object ArtistsView extends TableBuilder["artists_with_metadata", (
    "id" :=: Id,
    "name" :=: String,
    "tags" :=: Seq[String],
    "picture_url" :=: Option[String],
    "spotify_id" :=: Option[String],
    "lastfm_id" :=: Option[String]
  )]

  object Venues extends TableBuilder["venues", (
    "id" :=: Id,
    "location" :=: String,
    "name" :=: String,
    "address" :=: Option[String],
    "geo_location" :=: Option[Geography]
  )]

  object VenueLookups extends TableBuilder["venue_lookups", (
    "id" :=: Id,
    "lookup" :=: Id
  )]

  object Lookups extends TableBuilder["lookups", (
    "id" :=: Id,
    "lookup" :=: Id,
    "resource" :=: String
  )]

  type GigsSchema = (
    "id" :=: Id,
    "venue_id" :=: Id,
    "date" :=: LocalDate,
    "external_link" :=: Option[String],
    "source" :=: String,
    "author_id" :=: Option[Id],
    "authored" :=: Instant,
    "published" :=: Option[Instant],
    "publisher_id" :=: Option[Id]
  )

  object Gigs extends TableBuilder["gigs", GigsSchema]

  object GigsArtists extends TableBuilder["gigs_artists", (
    "gig_id" :=: Id,
    "artist_id" :=: Id,
    "index" :=: Int
  )]

  object Users extends TableBuilder["users", (
    "id" :=: Id,
    "email" :=: Option[String],
    "first_name" :=: String,
    "middle_name" :=: Option[String],
    "last_name" :=: String,
    "picture_url" :=: Option[String],
  )]

  object UsersLogin extends TableBuilder["users_login", (
    "user_id" :=: Id,
    "provider" :=: Idp,
    "external_id" :=: String
  )]

  object Roles extends TableBuilder["roles", (
    "id" :=: Id,
    "description" :=: Option[String]
  )]

  object UsersRoles extends TableBuilder["users_roles", (
    "user_id" :=: Id,
    "role_id" :=: String
  )]

  object Permissions extends TableBuilder["roles", (
    "id" :=: String,
    "description" :=: Option[String]
  )]

  object RolesPermissions extends TableBuilder["roles_permissions", (
    "role_id" :=: String,
    "permission_id" :=: String
  )]
}
