package io.epifab.tydal.gigmein.domain.model

import java.time.{Instant, LocalDate}

case class Gig(
  id: Id,
  date: LocalDate,
  artists: Seq[Artist],
  venue: Venue,
  externalLink: Option[String],
  source: GigSource,
  author: Option[User],
  authored: Instant,
  published: Option[Instant],
  publisher: Option[User],
  userMeta: Option[GigUserMeta] = None
)

class GigSource private(val value: String)

object GigSource {
  val Songkick = new GigSource("songkick")
  val GigMeIn = new GigSource("gigmein")

  def apply(value: String): GigSource = value match {
    case Songkick.value => Songkick
    case GigMeIn.value => GigMeIn
    case _ => throw new IllegalArgumentException("Unknown source")
  }
}

case class GigUserMeta(score: Double, reaction: Option[Reaction])
