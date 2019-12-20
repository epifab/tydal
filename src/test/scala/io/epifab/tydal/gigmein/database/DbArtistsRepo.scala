package io.epifab.tydal.gigmein.database

import io.epifab.tydal._
import io.epifab.tydal.gigmein.database.Schema._
import io.epifab.tydal.gigmein.domain.model._
import io.epifab.tydal.gigmein.domain.repos.ArtistsRepo
import io.epifab.tydal.queries.{Ascending, Delete, Insert, Select}
import io.epifab.tydal.runtime.{ReadStatement, Transaction}
import io.epifab.tydal.schema.Count
import shapeless.{::, HNil, Generic}

object DbArtistsRepo extends ArtistsRepo[Transaction] {

  def lookupArtist(lookupKey: Id): Transaction[Option[Artist]] =
    lookupArtistQuery.run(Tuple1("key" ~~> lookupKey))

  def findArtistById(artistId: Id): Transaction[Option[Artist]] =
    artistsByIdQuery.run(Tuple1("id" ~~> artistId))

  def findArtistsByGigIds(gigIds: Seq[Id]): Transaction[Map[Id, Seq[Artist]]] =
    artistsByGigIds
      .run(Tuple1("gig_ids" ~~> gigIds.toSeq))
      .map(_.groupMap(_._1)(_._2))

  def findArtistsByName(name: String, pageOffset: Long, pageSize: Int): Transaction[Seq[Artist]] =
    artistsByNameQuery.run(Tuple1("name" ~~> s"%$name%"))

  def countArtistsByName(name: String): Transaction[Long] =
    countArtistsQuery
      .run(Tuple1("name" ~~> name))
      .map(_.head._1)

  def addArtist(lookupKey: Option[Id], artist: ArtistBasicInfo): Transaction[Unit] =
    for {
      _ <- insertArtist(artist)
      _ <- lookupKey.fold(Transaction.successful(0))(insertArtistLookup(_, artist))
    } yield ()

  def removeArtist(artistId: Id): Transaction[Unit] =
    deleteArtistQuery
      .run(Tuple1("id" ~~> artistId))
      .discardResults

  private val artistsByIdQuery =
    Select
      .from(ArtistsView as "av")
      .take(_("av").*)
      .where(_ ("av", "id") === "id")
      .compile
      .to[Artist]
      .asOption

  private val artistsByNameQuery =
    Select
      .from(ArtistsView as "av")
      .take(_("av").*)
      .where(_ ("av", "name") like "name")
      .compile
      .to[Artist]
      .as[Vector]

  private val lookupArtistQuery =
    Select
      .from(ArtistsView as "av")
      .innerJoin(ArtistLookups as "al").on(_ ("id") === _ ("av", "id"))
      .take(_ ("av").*)
      .where(_ ("al", "lookup") === "key")
      .compile
      .to[Artist]
      .asOption

  private def insertArtistLookup(key: Id, artist: ArtistBasicInfo) =
    Insert
      .into(ArtistLookups)
      .compile
      .run((
        "id" ~~> artist.id,
        "lookup" ~~> key
      ))

  private def insertArtist(artist: ArtistBasicInfo) =
    Insert
      .into(Artists)
      .compile
      .run((
        "id" ~~> artist.id,
        "name" ~~> artist.name
      ))

  private val deleteArtistQuery =
    Delete
      .from(Artists)
      .where(_ ("id") === "id")
      .compile

  private val countArtistsQuery: ReadStatement[("name" ~~> String) :: HNil, Tuple1[Long], Option] =
    Select
      .from(Artists as "a")
      .take1($ => Count($("a", "id")) as "num_artists")
      .where(_("a", "name") === "name")
      .compile
      .toTuple
      .asOption

  private val artistsByGigIds: ReadStatement[("gig_ids" ~~> Seq[Id]) :: HNil, (Id, Artist), Vector] =
    Select
      .from(ArtistsView as "av")
      .innerJoin(GigsArtists as "ga").on(_("artist_id") === _("av", "id"))
      .where(_("ga", "gig_id") in "gig_ids")
      .take1(_("ga", "gig_id"))
      .alsoTake(_("av").*)
      .sortBy($ => Ascending($("ga", "gig_id")) -> Ascending($("ga", "index")))
      .compile
      .rawTo(hlist => hlist.head -> Generic[Artist].from(hlist.tail))
      .as[Vector]
}
