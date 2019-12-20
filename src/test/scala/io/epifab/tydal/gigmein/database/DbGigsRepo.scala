package io.epifab.tydal.gigmein.database

import java.time.{Instant, LocalDate}

import io.epifab.tydal._
import io.epifab.tydal.gigmein.database.Schema._
import io.epifab.tydal.gigmein.domain.model._
import io.epifab.tydal.gigmein.domain.repos.GigsRepo
import io.epifab.tydal.gigmein.domain.repos.GigsRepo.SearchCriteria
import io.epifab.tydal.queries._
import io.epifab.tydal.runtime.{ReadStatement, ResultSet, Transaction}
import io.epifab.tydal.schema.Postgis._
import io.epifab.tydal.schema._
import shapeless.{::, HNil}


object DbGigsRepo extends GigsRepo[Transaction] {
  implicit class ExtendedGigSeq(gigTransaction: Transaction[Vector[Gig]]) {
    def fetchArtists: Transaction[Vector[Gig]] = for {
      gigs <- gigTransaction
      artists <- DbArtistsRepo.findArtistsByGigIds(gigs.map(_.id))
    } yield gigs.map(gigWithoutArtists => gigWithoutArtists.copy(artists = artists.getOrElse(gigWithoutArtists.id, Seq.empty)))
  }

  implicit class ExtendedGigOption(gigTransaction: Transaction[Option[Gig]]) {
    def fetchArtists: Transaction[Option[Gig]] = for {
      gigs <- gigTransaction
      artists <- DbArtistsRepo.findArtistsByGigIds(gigs.map(_.id).toSeq)
    } yield gigs.map(gigWithoutArtists => gigWithoutArtists.copy(artists = artists.getOrElse(gigWithoutArtists.id, Seq.empty)))
  }

  override def findGigByExternalLink(uri: String): Transaction[Option[Gig]] =
    findGigByUriStmt
      .run(Tuple1("uri" ~~> Some(uri)))
      .fetchArtists

  override def findGigById(user: Option[User], id: Id): Transaction[Option[Gig]] =
    findGigByIdQuery(user)
      .run(Tuple1("id" ~~> id))
      .fetchArtists

  override def findGigs(user: Option[User], searchCriteria: GigsRepo.SearchCriteria): Transaction[Seq[Gig]] =
    findGigsQuery(user, searchCriteria)
      .inRange["offset", "limit"]
      .compile
      .to(rs => gigFromResultSet(rs))
      .as[Vector]
      .run((
        "offset" ~~> (searchCriteria.pageOffset * searchCriteria.pageSize),
        "limit" ~~> searchCriteria.pageSize
      ))
      .fetchArtists

  override def countGigs(user: Option[User], searchCriteria: GigsRepo.SearchCriteria): Transaction[Long] = {
    findGigsQuery(user, searchCriteria)
      .take1($ => Count(Distinct($("g", "id"))) as "num_gigs")
      .groupBy1(_("g", "id"))
      .compile
      .head
      .single
      .run(())
  }

  override def addGig(gig: Gig): Transaction[Unit] = {
    val gigTransaction = Insert.into(Gigs)
      .compile
      .run((
        "id" ~~> gig.id,
        "venue_id" ~~> gig.venue.id,
        "date" ~~> gig.date,
        "external_link" ~~> gig.externalLink,
        "source" ~~> gig.source.value,
        "author_id" ~~> gig.author.map(_.id),
        "authored" ~~> gig.authored,
        "published" ~~> gig.published,
        "publisher_id" ~~> gig.publisher.map(_.id)
      ))

    val artistsTransactions = gig.artists.zipWithIndex.map { case (artist, index) =>
      Insert
        .into(GigsArtists)
        .compile.run((
          "gig_id" ~~> gig.id,
          "artist_id" ~~> artist.id,
          "index" ~~> index
        ))
    }

    for {
      _ <- gigTransaction
      _ <- Transaction.list(artistsTransactions.toList)
    } yield ()
  }

  override def removeGig(gigId: Id): Transaction[Unit] =
    Delete
      .from(Gigs)
      .where(_("id") === "id")
      .compile
      .run(Tuple1("id" ~~> gigId))
      .discardResults

  override def publishGig(gigId: Id, publisher: User, published: Instant): Transaction[Unit] =
    Update(Gigs)
      .fields($ => $("published") -> $("publisher_id"))
      .where(_("id") === "id")
      .compile
      .run((
        "published" ~~> Some(published),
        "publisher_id" ~~> Some(publisher.id),
        "id" ~~> gigId,
      ))
      .discardResults

  private def findGigsQuery(loggedInUser: Option[User], searchCriteria: SearchCriteria) = {
    findGigBaseQuery
      .innerJoin(GigsArtists as "ga").on(_("gig_id") === _("g", "id"))
      .innerJoin(ArtistsView as "a").on(_("id") === _("ga", "artist_id"))
      .focus("v").alsoTake(v => Tuple1(
        Distance(
          v("geo_location"),
          MakePoint(
            Literal(searchCriteria.maxDistance.map(_._1.lat)),
            Literal(searchCriteria.maxDistance.map(_._1.lng))
          ).castTo[Geography]
        ) as "distance"
      ))
      .groupBy(_ => findGigBaseQuery.*)
      .focus("g", "v", "a").where { case (g, v, a) =>
        val locationFilter = searchCriteria.location
          .map(location => v("location") === Literal(location.name)).toFilter

        val startDateFilter = searchCriteria.startDate
          .map(date => g("date") >= Literal(date)).toFilter

        val endDateFilter = searchCriteria.endDate
          .map(date => g("date") < Literal(date)).toFilter

        val queryFilter = searchCriteria.query
          .map(query => a("name") like Literal(s"%$query%") or (v("name") like Literal(s"%$query%"))).toFilter

        val tagsFilter = (searchCriteria.tags match {
          case empty if empty.isEmpty => None
          case nonEmpty => Some(a("tags") overlaps Literal(nonEmpty))
        }).toFilter

        val distanceFilter = searchCriteria.maxDistance.map { case (coordinates, distance) =>
          Distance(
            v("geo_location"),
            MakePoint(Literal(coordinates.lat), Literal(coordinates.lng)).castTo[Geography]
          ) < Literal(distance.meters)
        }.toFilter

        locationFilter and
          startDateFilter and
          endDateFilter and
          queryFilter and
          distanceFilter and
          tagsFilter and
          entitlementsFilter(loggedInUser, g("author_id"), g("published"))
      }
  }

  private def entitlementsFilter[AuthorId <: Field[Option[Id]], Published <: Field[Option[Instant]]](loggedInUser: Option[User], authorId: AuthorId, published: Published) = {
    val isSuperUser = loggedInUser
      .map(user => Literal(user.id) in superUsers).toFilter
    val isAuthor = loggedInUser
      .map(user => authorId === Literal(user.id)).toFilter

    isSuperUser or isAuthor or published.isDefined
  }

  private val superUsers =
    Select
      .from(UsersRoles as "ur")
      .innerJoin(RolesPermissions as "rp").on(_ ("role_id") === _ ("ur", "role_id"))
      .where(_("rp", "permission_id") === Literal(Permission.GigsViewAll.name))
      .take1(_ ("ur", "user_id"))

  private val findGigBaseQuery =
    Select
      .from(Gigs as "g")
      .innerJoin(Venues as "v").on(_ ("id") === _ ("g", "venue_id"))
      .take(_ ("g").*)
      .focus("v").alsoTake(v => (
        v("location") as "venue_location",
        v("name") as "venue_name",
        v("address") as "venue_address",
        Latitude(v("geo_location").castTo[Geometry]) as "latitude",
        Longitude(v("geo_location").castTo[Geometry]) as "longitude"
      ))

  private val findGigByUriStmt: ReadStatement[("uri" ~~> Option[String]) :: HNil, Gig, Option] =
    findGigBaseQuery
      .alsoTake(_ => Tuple1(Literal[Option[Double]](None) as "distance"))
      .where(_ ("g", "external_link") === "uri")
      .compile
      .to(resultSet => gigFromResultSet(resultSet))
      .asOption

  private def findGigByIdQuery(user: Option[User]): ReadStatement[("id" ~~> Id) :: HNil, Gig, Option] =
    findGigBaseQuery
      .alsoTake(_ => Tuple1(Literal[Option[Double]](None) as "distance"))
      .focus("g").where(g => g("id") === "id" and entitlementsFilter(user, g("author_id"), g("published")))
      .compile
      .to(resultSet => gigFromResultSet(resultSet))
      .asOption

  type GigQueryResults =
    (Id As "id") ::
    (Id As "venue_id") ::
    (LocalDate As "date") ::
    (Option[String] As "external_link") ::
    (String As "source") ::
    (Option[Id] As "author_id") ::
    (Instant As "authored") ::
    (Option[Instant] As "published") ::
    (Option[Id] As "publisher_id") ::
    (String As "venue_location") ::
    (String As "venue_name") ::
    (Option[String] As "venue_address") ::
    (Option[Double] As "latitude") ::
    (Option[Double] As "longitude") ::
    (Option[Double] As "distance") ::
    HNil

  private def gigFromResultSet(results: ResultSet[GigQueryResults]): Gig =
    Gig(
      results("id"),
      results("date"),
      Seq.empty,
      Venue(
        results("venue_id"),
        results("venue_location"),
        results("venue_name"),
        results("venue_address"),
        for {
          lat <- results("latitude")
          lng <- results("longitude")
        } yield Coordinates(lat, lng),
        results("distance")
      ),
      results("external_link"),
      GigSource(results("source")),
      results("author_id").map(UserId),
      results("authored"),
      results("published"),
      results("publisher_id").map(UserId),
      None
    )
}
