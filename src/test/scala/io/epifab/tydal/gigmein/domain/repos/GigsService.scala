package io.epifab.tydal.gigmein.domain.repos

import io.epifab.tydal.gigmein.domain.model.{Gig, Id, User}
import io.epifab.tydal.gigmein.domain.repos.GigsRepo.SearchCriteria

case class GigsResult(gigs: Seq[Gig], length: Long)

trait GigsService[F[_]] {
  def findGig(user: Option[User], id: Id): F[Option[Gig]]
  def findGigs(user: Option[User], searchCriteria: SearchCriteria): F[GigsResult]
}
