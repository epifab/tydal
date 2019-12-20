package io.epifab.tydal.gigmein.database

import io.epifab.tydal.FunctionalTestBase
import io.epifab.tydal.gigmein.domain.model.{Idp, UserLoginInfo}
import io.epifab.tydal.gigmein.fixtures.TestData
import org.scalatest.{FlatSpec, Matchers}

class DbUsersRepoSpec extends FlatSpec with FunctionalTestBase with Matchers with TestData {

  "The DBUsersRepo" should "create and find a user by id" in {
    val userLogin = UserLoginInfo(anAuthor.id, Idp.spotify, "t3st!123456")

    val results = (for {
      _ <- DbUsersRepo.addUser(anAuthor)
      _ <- DbUsersRepo.ensureRoles(anAuthor)
      _ <- DbUsersRepo.addUserLogin(userLogin)
      createdUser <- DbUsersRepo.findUserById(anAuthor.id)
      loggedInUser <- DbUsersRepo.findUserByIdp(userLogin.provider, userLogin.externalId)
      _ <- DbUsersRepo.removeUser(anAuthor.id)
      deleteUser <- DbUsersRepo.findUserById(anAuthor.id)
    } yield (createdUser, loggedInUser, deleteUser)).runSync()

    results shouldBe Right((Some(anAuthor), Some(anAuthor), None))
  }
}
