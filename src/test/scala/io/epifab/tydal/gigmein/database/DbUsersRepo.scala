package io.epifab.tydal.gigmein.database

import io.epifab.tydal._
import io.epifab.tydal.gigmein.database.Schema._
import io.epifab.tydal.gigmein.domain.model._
import io.epifab.tydal.gigmein.domain.repos.UsersRepo
import io.epifab.tydal.queries.{Delete, Insert, Select, Update}
import io.epifab.tydal.runtime.{ReadStatement, Transaction, WriteStatement}
import shapeless.{::, HNil}

import scala.collection.MapView

object DbUsersRepo extends UsersRepo[Transaction] {

  override def addUser(user: UserPersonalInfo): Transaction[Unit] =
    insertUserStmt
      .run((
        "id" ~~> user.id,
        "email" ~~> user.email,
        "first_name" ~~> user.firstName,
        "middle_name" ~~> user.middleName,
        "last_name" ~~> user.lastName,
        "picture_url" ~~> user.pictureUrl
      ))
      .discardResults

  override def updateUser(user: UserPersonalInfo): Transaction[Unit] =
    updateUserStmt
      .run((
        "email" ~~> user.email,
        "first_name" ~~> user.firstName,
        "middle_name" ~~> user.middleName,
        "last_name" ~~> user.lastName,
        "picture_url" ~~> user.pictureUrl,
        "id" ~~> user.id
      ))
      .discardResults

  override def removeUser(id: Id): Transaction[Unit] = {
    val placeholders = Tuple1("id" ~~> id)

    for {
      _ <- deleteUserLoginStmt.run(placeholders)
      _ <- deleteUserStmt.run(placeholders)
    } yield ()
  }

  override def findUserById(id: Id): Transaction[Option[UserWithRoles]] =
    findUserWithRolesByIdStmt
      .run(Tuple1("uid" ~~> id))
      .map(groupUserRoles(_).headOption)

  override def findUserByIdp(provider: Idp, externalId: String): Transaction[Option[UserWithRoles]] =
    findUserWithRolesByIdpStmt
      .run(("idp" ~~> provider, "idp_id" ~~> externalId))
      .map(groupUserRoles(_).headOption)

  override def addUserLogin(user: UserLoginInfo): Transaction[Unit] =
    Insert
      .into(UsersLogin)
      .compile
      .run((
        "user_id" ~~> user.id,
        "provider" ~~> user.provider,
        "external_id" ~~> user.externalId
      ))
      .discardResults

  override def ensureRoles(userId: Id, roleIds: Set[String]): Transaction[Unit] =
    for {
      currentRoleIds <- userRolesStmt.run(Tuple1("uid" ~~> userId)).map(_.toSet)
      _ <- currentRoleIds -- roleIds match {
        case roles if roles.isEmpty => Transaction.successful(0)
        case roles if roles.nonEmpty => deleteRolesStmt.run(("uid" ~~> userId, "role_ids" ~~> roles.toSeq))
      }
      _ <- roleIds -- currentRoleIds match {
        case roles if roles.isEmpty => Transaction.successful(0)
        case roles if roles.nonEmpty => Transaction.list(roles.map(rid => insertRoleStmt.run(("user_id" ~~> userId, "role_id" ~~> rid))).toList)
      }
    } yield ()

  private val insertUserStmt: WriteStatement[("id" ~~> Id) :: ("email" ~~> Option[String]) :: ("first_name" ~~> String) :: ("middle_name" ~~> Option[String]) :: ("last_name" ~~> String) :: ("picture_url" ~~> Option[String]) :: HNil, HNil] =
    Insert.into(Users).compile

  private val updateUserStmt: WriteStatement[("email" ~~> Option[String]) :: ("first_name" ~~> String) :: ("middle_name" ~~> Option[String]) :: ("last_name" ~~> String) :: ("picture_url" ~~> Option[String]) :: ("id" ~~> Id) :: HNil, HNil] =
    Update(Users)
      .fields(u => (
        u("email"),
        u("first_name"),
        u("middle_name"),
        u("last_name"),
        u("picture_url")
      ))
      .where(_("id") === "id")
      .compile

  private val userRolesStmt: ReadStatement[("uid" ~~> Id) :: HNil, String, Vector] =
    Select
      .from(UsersRoles as "ur")
      .take1(_("ur", "role_id"))
      .where(_("ur", "user_id") === "uid")
      .compile
      .rawTo(_.head)
      .as[Vector]

  private val deleteRolesStmt: WriteStatement[("uid" ~~> Id) :: ("role_ids" ~~> Seq[String]) :: HNil, HNil] =
    Delete
      .from(UsersRoles)
      .where(ur => (ur("user_id") === "uid") and (ur("role_id") in "role_ids"))
      .compile

  private val insertRoleStmt: WriteStatement[("user_id" ~~> Id) :: ("role_id" ~~> String) :: HNil, HNil] =
    Insert
      .into(UsersRoles)
      .compile

  private val findUserWithRolesBaseStmt =
    Select
      .from(Users as "u")
      .leftJoin(UsersRoles as "ur").on(_ ("user_id") === _ ("u", "id"))
      .leftJoin(RolesPermissions as "rp").on(_ ("role_id") === _ ("ur", "role_id"))
      .focus("u", "rp").take { case (u, rp) => (
        rp("role_id"),
        rp("permission_id"),
        u("id"),
        u("email"),
        u("first_name"),
        u("middle_name"),
        u("last_name"),
        u("picture_url")
      )}

  private val findUserWithRolesByIdStmt: ReadStatement[("uid" ~~> Id) :: HNil, (UserPersonalInfo, Option[Role]), Vector] =
    findUserWithRolesBaseStmt
      .where(_("u", "id") === "uid")
      .compile
      .rawTo { case roleId :: permissionId :: user =>
        (UserPersonalInfo.apply _).tupled(user.tupled) ->
          roleId.map(Role(_, permissionId.map(Permission(_)).toSeq))
      }
      .as[Vector]

  private val findUserWithRolesByIdpStmt: ReadStatement[("idp" ~~> Idp) :: ("idp_id" ~~> String) :: HNil, (UserPersonalInfo, Option[Role]), Vector] =
    findUserWithRolesBaseStmt
      .innerJoin(UsersLogin as "ul").on(_("user_id") === _("u", "id"))
      .focus("ul").where(ul => (ul("provider") === "idp") and (ul("external_id") === "idp_id"))
      .compile
      .rawTo { case roleId :: permissionId :: user =>
        (UserPersonalInfo.apply _).tupled(user.tupled) ->
          roleId.map(Role(_, permissionId.map(Permission(_)).toSeq))
      }
      .as[Vector]

  private def groupUserRoles(usersRole: Vector[(UserPersonalInfo, Option[Role])]): Iterable[UserWithRoles] = {
    val rawMap: MapView[UserPersonalInfo, Iterable[Role]] =
      usersRole.groupMap(_._1)(_._2).view.mapValues(roles =>
        roles.flatten.groupMap(_.name)(_.permissions).map { case (roleId, permissions) =>
          Role(roleId, permissions.flatten)
        }
      )

    rawMap.map {
      case (userInfo, roles) => UserWithRoles(
        userInfo.id,
        userInfo.email,
        userInfo.firstName,
        userInfo.middleName,
        userInfo.lastName,
        userInfo.pictureUrl,
        roles.toSeq
      )
    }
  }

  val deleteUserStmt: WriteStatement[("id" ~~> Id) :: HNil, HNil] = Delete
    .from(Users)
    .where(_ ("id") === "id")
    .compile

  val deleteUserLoginStmt: WriteStatement[("id" ~~> Id) :: HNil, HNil] = Delete
    .from(UsersLogin)
    .where(_ ("user_id") === "id")
    .compile

}
