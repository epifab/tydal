package io.epifab.tydal.gigmein.domain.repos

import io.epifab.tydal.gigmein.domain.model._

trait UsersRepo[F[_]] {
  def addUser(user: UserPersonalInfo): F[Unit]
  def updateUser(user: UserPersonalInfo): F[Unit]
  def removeUser(id: Id): F[Unit]

  def findUserById(id: Id): F[Option[UserWithRoles]]
  def findUserByIdp(provider: Idp, externalId: String): F[Option[UserWithRoles]]

  def addUserLogin(user: UserLoginInfo): F[Unit]
  def ensureRoles(userId: Id, roleIds: Set[String]): F[Unit]
  def ensureRoles(user: UserWithRoles): F[Unit] = ensureRoles(user.id, user.roles.map(_.name).toSet)
}
