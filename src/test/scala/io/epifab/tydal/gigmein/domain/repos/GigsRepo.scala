package io.epifab.tydal.gigmein.domain.repos

import java.time.{Instant, LocalDate}

import io.epifab.tydal.gigmein.database.DbGigsRepo
import io.epifab.tydal.gigmein.domain.model._
import io.epifab.tydal.gigmein.domain.repos.GigsRepo.SearchCriteria
import io.epifab.tydal.runtime.Transaction

trait GigsRepo[F[_]] {
  def findGigByExternalLink(uri: String): F[Option[Gig]]
  def findGigById(user: Option[User], id: Id): F[Option[Gig]]
  def findGigs(user: Option[User], searchCriteria: SearchCriteria): F[Seq[Gig]]
  def countGigs(user: Option[User], searchCriteria: SearchCriteria): F[Long]
  def addGig(gig: Gig): F[Unit]
  def removeGig(gigId: Id): F[Unit]
  def publishGig(gigId: Id, publisher: User, published: Instant): F[Unit]
}

object GigsRepo {
  trait Distance {
    def meters: Int
  }

  object Distance {
    def meters(value: Int): Distance = new Distance {
      override def meters: Int = value
    }
  }

  case class SearchCriteria(
    location: Option[Location] = None,
    startDate: Option[LocalDate] = None,
    endDate: Option[LocalDate] = None,
    tags: Seq[String] = Seq.empty,
    query: Option[String] = None,
    maxDistance: Option[(Coordinates, Distance)] = None,
    authorId: Option[Id] = None,
    pageOffset: Long = 0,
    pageSize: Int = 20
  )
}

object DbGigsService extends GigsService[Transaction] {
  override def findGig(user: Option[User], id: Id): Transaction[Option[Gig]] =
    DbGigsRepo.findGigById(user, id)

  override def findGigs(user: Option[User], searchCriteria: SearchCriteria): Transaction[GigsResult] =
    for {
      count <- DbGigsRepo.countGigs(user, searchCriteria)
      gigs <- DbGigsRepo.findGigs(user, searchCriteria)
    } yield GigsResult(gigs, count)
}
