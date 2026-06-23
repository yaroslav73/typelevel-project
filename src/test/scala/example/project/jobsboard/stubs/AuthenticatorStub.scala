package example.project.jobsboard.stubs

import example.project.jobsboard.domain.Aliases.Authenticator
import tsec.mac.jca.HMACSHA256
import tsec.authentication.IdentityStore
import cats.effect.IO
import example.project.jobsboard.domain.User
import cats.data.OptionT
import example.project.jobsboard.fixtures.UserFixture
import tsec.authentication.JWTAuthenticator
import concurrent.duration.DurationInt
import cats.effect.kernel.Sync

object AuthenticatorStub extends UserFixture:
  def apply[F[_]: Sync](): Authenticator[F] = {
    // 1. Key for hashing
    val key = HMACSHA256.unsafeGenerateKey

    // 2. Indentity store to retrieve users
    val idStore: IdentityStore[F, String, User] = (email: String) =>
      email match {
        case john.email => OptionT.pure(john)
        case anna.email => OptionT.pure(anna)
        case _          => OptionT.none[F, User]
      }

    // 3. jwt authenticator
    JWTAuthenticator.unbacked.inBearerToken(
      1.day,
      None,
      idStore,
      key,
    )
  }
