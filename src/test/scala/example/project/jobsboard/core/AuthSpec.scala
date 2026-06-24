package example.project.jobsboard.core

import cats.implicits.*
import org.scalatest.matchers.should.Matchers
import example.project.jobsboard.fixtures.UserFixture
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AsyncFreeSpec
import cats.effect.IO
import java.util.UUID
import example.project.jobsboard.domain.User
import example.project.jobsboard.domain.Aliases.Authenticator
import tsec.mac.jca.HMACSHA256
import tsec.authentication.IdentityStore
import cats.data.OptionT
import tsec.authentication.JWTAuthenticator
import concurrent.duration.DurationInt
import example.project.jobsboard.domain.Auth.NewPasswordInfo

class AuthSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers with UserFixture {
  "Auth 'algebra'" - {
    "login should return None if the user does not exists" in {
      val token = auth.login(NotFoundUserEmail, john.hashedPassword)

      token.asserting(_ shouldBe None)
    }

    "login should return None if the user password is wrong" in {
      val token = auth.login(john.email, "wrongpassword")

      token.asserting(_ shouldBe None)
    }

    "login should return token if the user exists and password is correct" in {
      val token = auth.login(john.email, "password1")

      token.asserting(_ shouldBe defined)
    }

    "signup should not create user if the email already exists" in {
      val userId = auth.signUp(
        User.New.recruiter(
          email     = john.email,
          password  = "123456789",
          firstName = Some("Test"),
          lastName  = Some("Name"),
          company   = Some("Test Company"),
        )
      )

      userId.asserting(_ shouldBe None)
    }

    "signup should create user" in {
      val auth = Auth.of[IO](testUsers)
      val userId = auth.signUp(
        User.New.recruiter(
          email     = NotFoundUserEmail,
          password  = "123456789",
          firstName = Some("Test"),
          lastName  = Some("Name"),
          company   = Some("Test Company"),
        )
      )

      userId.asserting(_ shouldBe defined)
    }

    "changePassword should return error if user does not exists" in {
      val result = auth.changePassword(NotFoundUserEmail, NewPasswordInfo("oldpassword", "newpassword"))

      result.asserting(_ shouldBe Left("User with this email not found"))
    }

    "changePassword should return error if user old password does not match" in {
      val result = auth.changePassword(john.email, NewPasswordInfo("oldpassword", "newpassword"))

      result.asserting(_ shouldBe Left("Invalid password"))
    }

    "changePassword should change password if user exists" in {
      val result = auth.changePassword(john.email, NewPasswordInfo("password1", "newpassword"))

      result.asserting(_ shouldBe Right(Some(john)))
    }
  }

  private val testUsers: Users[IO] = new Users[IO] {
    def find(userId: UUID): IO[Option[User]] =
      if (userId == john.id) IO.pure(Some(john)) else IO.pure(None)
    def find(email: String): IO[Option[User]] =
      if (email == john.email) IO.pure(Some(john)) else IO.pure(None)
    def create(user: User.New): IO[UUID] =
      IO.pure(john.id)
    def update(user: User): IO[Option[User]] =
      if (user.id == john.id) IO.pure(Some(john)) else IO.pure(None)
    def delete(id: UUID): IO[Boolean] =
      if (id == john.id) IO.pure(true) else IO.pure(false)
  }

  private val authenticator: Authenticator[IO] = {
    // 1. Key for hashing
    val key = HMACSHA256.unsafeGenerateKey
    // 2. Indentity store to retrieve users
    val idStore: IdentityStore[IO, String, User] = (email: String) =>
      email match {
        case john.email => OptionT.pure(john)
        case anna.email => OptionT.pure(anna)
        case _          => OptionT.none[IO, User]
      }
    // 3. jwt authenticator
    JWTAuthenticator.unbacked.inBearerToken(
      1.day,
      None,
      idStore,
      key,
    )
  }

  private val auth = Auth.of[IO](testUsers)
}
