package example.project.jobsboard.domain

import tsec.authentication.AugmentedJWT
import tsec.authentication.JWTAuthenticator
import tsec.mac.jca.HMACSHA256
import tsec.authentication.SecuredRequest
import org.http4s.Response
import tsec.authorization.BasicRBAC
import cats.implicits.*
import example.project.jobsboard.domain.User.Role
import tsec.authorization.SimpleAuthEnum
import tsec.authorization.AuthorizationInfo
import cats.MonadError
import tsec.authorization.AuthGroup.Type
import tsec.authorization.AuthGroup
import cats.Applicative
import cats.MonadThrow
import tsec.authentication.TSecAuthService
import cats.Monad
import org.http4s.Status
import cats.kernel.Semigroup
import tsec.authentication.SecuredRequestHandler

object Aliases {
  type Crypto              = HMACSHA256
  type JwtToken            = AugmentedJWT[Crypto, String]
  type Authenticator[F[_]] = JWTAuthenticator[F, String, User, Crypto]
  type AuthRoute[F[_]]     = PartialFunction[SecuredRequest[F, User, JwtToken], F[Response[F]]]
  type AuthRBAC[F[_]]      = BasicRBAC[F, Role, User, JwtToken]

  type SecuredHandler[F[_]] = SecuredRequestHandler[F, String, User, JwtToken]
  object SecuredHandler {
    def apply[F[_]](using handler: SecuredHandler[F]): SecuredHandler[F] = handler
  }

  // RBAC
  // BasicRBAC[F, Role, User, JwtToken]
  def allRoles[F[_]: MonadThrow]: BasicRBAC[F, Role, User, JwtToken] =
    BasicRBAC.all[F, Role, User, JwtToken]

  def adminOnly[F[_]: MonadThrow]: BasicRBAC[F, Role, User, JwtToken] =
    BasicRBAC(Role.ADMIN)

  def recruiterOnly[F[_]: MonadThrow]: BasicRBAC[F, Role, User, JwtToken] =
    BasicRBAC(Role.RECRUITTER)

  given roleAuthEnum: SimpleAuthEnum[Role, String] with
    override protected val values: AuthGroup[Role] = AuthGroup.fromSet(Role.values.toSet)
    def getRepr(a: Role): String = a.toString()

  given authInfo[F[_]: Applicative]: AuthorizationInfo[F, Role, User] with
    def fetchInfo(u: User): F[Role] = u.role.pure[F]

  final case class Authorizations[F[_]](rbacRoutes: Map[AuthRBAC[F], List[AuthRoute[F]]])
  object Authorizations:
    given combiner[F[_]]: Semigroup[Authorizations[F]] = Semigroup.instance { (a, b) =>
      Authorizations(a.rbacRoutes |+| b.rbacRoutes)
    }

  extension [F[_]](route: AuthRoute[F])
    def restrictedTo(rbac: AuthRBAC[F]): Authorizations[F] =
      Authorizations(Map(rbac -> List(route)))

  given auth2Tsec[F[_]: Monad]: Conversion[Authorizations[F], TSecAuthService[User, JwtToken, F]] =
    auths => {
      val unauthorizedService: TSecAuthService[User, JwtToken, F] = TSecAuthService[User, JwtToken, F] { _ =>
        Response[F](Status.Unauthorized).pure[F]
      }

      auths.rbacRoutes.toSeq
        .foldLeft(unauthorizedService) {
          case (acc, (rbac, routes)) =>
            val bigRoute = routes.reduce(_.orElse(_))
            TSecAuthService.withAuthorizationHandler(rbac)(bigRoute, acc.run)
        }
    }
}
