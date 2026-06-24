package example.project.jobsboard.fixtures

import example.project.jobsboard.domain.Aliases.SecuredHandler
import cats.effect.IO
import tsec.authentication.SecuredRequestHandler
import example.project.jobsboard.stubs.AuthenticatorStub
import example.project.jobsboard.domain.Aliases.Authenticator

trait SecuredFixture {
  val authenticator: Authenticator[IO] = AuthenticatorStub[IO]()

  given securedHandlerIO: SecuredHandler[IO] = SecuredRequestHandler(authenticator)
}
