package io.epifab.tydal.gigmein.domain.model

import java.time.LocalDateTime

case class Idp(value: String) {
  override def toString: String = value
}

object Idp {
  val facebook: Idp = new Idp("facebook")
  val spotify: Idp = new Idp("spotify")

  def get(name: String): Either[String, Idp] = name match {
    case facebook.value => Right(facebook)
    case spotify.value => Right(spotify)
    case unknown => Left(s"Unknown identity provider: $unknown")
  }
}

sealed trait User {
  def id: Id

  override def equals(obj: Any): Boolean = obj match {
    case u: User => u.id == id
    case _ => false
  }
}

sealed trait UserLoginInfo extends User {
  def provider: Idp
  def externalId: String
}

object UserLoginInfo {
  case class UserIDPInfoImpl(id: Id, provider: Idp, externalId: String) extends UserLoginInfo

  def apply(userId: Id, provider: Idp, externalId: String): UserLoginInfo =
    UserIDPInfoImpl(userId, provider, externalId)
}

sealed trait UserPersonalInfo extends User {
  def email: Option[String]
  def firstName: String
  def middleName: Option[String]
  def lastName: String
  def pictureUrl: Option[String]
}

object UserPersonalInfo {
  case class UserPersonalInfoImpl(
    id: Id,
    email: Option[String],
    firstName: String,
    middleName: Option[String],
    lastName: String,
    pictureUrl: Option[String]
  ) extends UserPersonalInfo

  def apply(
    id: Id,
    email: Option[String],
    firstName: String,
    middleName: Option[String],
    lastName: String,
    pictureUrl: Option[String]
  ): UserPersonalInfo = UserPersonalInfoImpl(id, email, firstName, middleName, lastName, pictureUrl)
}

sealed trait UserPermissionInfo {
  def roles: Seq[Role]
}

object User {
  def apply(userId: Id): User = UserId(userId)
}

case class UserId(id: Id) extends User

case class UserWithRoles(
  id: Id,
  email: Option[String],
  firstName: String,
  middleName: Option[String],
  lastName: String,
  pictureUrl: Option[String],
  roles: Seq[Role]
) extends UserPersonalInfo with UserPermissionInfo {
  def hasPermission(permission: Permission): Boolean =
    roles.exists(_.permissions.contains(permission))
}

case class Role(name: String, permissions: Seq[Permission])

case class Permission private(name: String)

object Permission {
  val GigsCreate = new Permission("gigs-create")
  val GigsViewAll = new Permission("gigs-view-all")
  val GigsPublish = new Permission("gigs-publish")
  val GigsUnPublish = new Permission("gigs-unpublish")

  private val all = Seq(
    GigsCreate,
    GigsViewAll,
    GigsPublish,
    GigsUnPublish
  )

  def unsafeGet(name: String): Permission =
    get(name).getOrElse(throw new IllegalArgumentException(s"Unknown permission $name"))

  def get(name: String): Either[String, Permission] =
    all.find(_.name == name).toRight(s"Unknown permission $name")
}

case class Reaction(value: String)

object Reaction {
  val hate = Reaction("HATE")
  val love = Reaction("LOVE")
}

case class UserGigsReaction(
  user: UserWithRoles,
  gig: Gig,
  reaction: Reaction,
  timestamp: LocalDateTime
)
