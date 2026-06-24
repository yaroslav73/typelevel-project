package example.project.jobsboard.modules

import cats.effect.Concurrent
import cats.effect.kernel.Resource
import cats.syntax.all.*

import example.project.jobsboard.http.routes.{ HealthRoutes, JobRoutes }
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.typelevel.log4cats.Logger
import example.project.jobsboard.http.routes.AuthRoutes
import example.project.jobsboard.domain.Aliases.Authenticator
import cats.effect.Sync
import cats.effect.Ref
import tsec.authentication.IdentityStore
import example.project.jobsboard.domain.User
import cats.data.OptionT
import example.project.jobsboard.core.Users
import tsec.common.SecureRandomId
import example.project.jobsboard.domain.Aliases.JwtToken
import tsec.mac.jca.HMACSHA256
import example.project.jobsboard.config.SecurityConfig
import tsec.authentication.JWTAuthenticator
import tsec.authentication.BackingStore
import example.project.jobsboard.domain.Aliases.SecuredHandler
import tsec.authentication.SecuredRequestHandler

class HttpApi[F[_]: Concurrent: Logger] private (core: Core[F], authenticator: Authenticator[F]) extends Http4sDsl[F]:
  private given securedHandler: SecuredHandler[F] = SecuredRequestHandler(authenticator)

  private val healthRoutes: HttpRoutes[F] = HealthRoutes[F].routes
  private val jobRoutes: HttpRoutes[F]    = JobRoutes[F](core.jobs).routes
  private val authRoutes: HttpRoutes[F]   = AuthRoutes.of[F](core.auth, authenticator).routes

  val routes: HttpRoutes[F] = Router("/api" -> (healthRoutes <+> jobRoutes <+> authRoutes))

object HttpApi:
  def makeAuthenticator[F[_]: Sync](users: Users[F])(securityConfig: SecurityConfig): F[Authenticator[F]] = {
    // 1. Indentity store
    val idStore: IdentityStore[F, String, User] = (email: String) => OptionT(users.find(email))

    // 2. Backing store for JWT tokens
    val tokenStoreF = Ref.of[F, Map[SecureRandomId, JwtToken]](Map.empty).map { ref =>
      new BackingStore[F, SecureRandomId, JwtToken] {
        def put(token: JwtToken): F[JwtToken] = ref.modify(store => store + (token.id -> token) -> token)
        def get(id: SecureRandomId): OptionT[F, JwtToken] = OptionT(ref.get.map(_.get(id)))
        def update(token: JwtToken): F[JwtToken] = put(token)
        def delete(id: SecureRandomId): F[Unit] = ref.modify(store => (store - id, ()))
      }
    }

    // 3. Hashing key
    val keyF = HMACSHA256.buildKey[F](securityConfig.secret.getBytes("UTF-8"))

    // 4. Authenticator and 5. Auth
    for {
      key        <- keyF
      tokenStore <- tokenStoreF
      authenticator = JWTAuthenticator.backed.inBearerToken(
        expiryDuration = securityConfig.jwtExpiryDuration,
        maxIdle        = None,
        tokenStore     = tokenStore,
        identityStore  = idStore,
        signingKey     = key
      )
    } yield authenticator
  }

  def apply[F[_]: Sync: Concurrent: Logger](core: Core[F], securityConfig: SecurityConfig): Resource[F, HttpApi[F]] =
    Resource
      .eval(makeAuthenticator(core.users)(securityConfig))
      .map(authenticator => new HttpApi[F](core, authenticator))
