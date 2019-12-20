package io.epifab.tydal.gigmein.fixtures

import java.util.UUID

import io.epifab.tydal.gigmein.domain.model.{Permission, Role, UserWithRoles}

object Roles {
  val publishers = Role("publishers", Seq(
    Permission.GigsPublish,
    Permission.GigsUnPublish,
    Permission.GigsViewAll))

  val authors = Role("authors", Seq(
    Permission.GigsCreate
  ))
}

trait Users {
  val anAuthor = UserWithRoles(
    id = UUID.randomUUID,
    firstName = "John",
    middleName = Some("Jim"),
    lastName = "Doe",
    pictureUrl = Some("https://gigme.in/users/john-doe.jpg"),
    email = Some("johndoe@gigme.in"),
    roles = Seq(Roles.authors)
  )

  val anotherAuthor = UserWithRoles(
    id = UUID.randomUUID,
    firstName = "Jack",
    middleName = None,
    lastName = "Doe",
    pictureUrl = Some("https://gigme.in/users/jack-doe.jpg"),
    email = Some("janedoe@gigme.in"),
    roles = Seq(Roles.authors)
  )

  val aPublisher = UserWithRoles(
    id = UUID.randomUUID,
    firstName = "Jane",
    middleName = Some("Jade"),
    lastName = "Doe",
    pictureUrl = Some("https://gigme.in/users/jane-doe.jpg"),
    email = Some("janedoe@gigme.in"),
    roles = Seq(Roles.publishers)
  )
}
