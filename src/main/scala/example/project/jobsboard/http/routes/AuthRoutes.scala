package example.project.jobsboard.http.routes

import cats.effect.Concurrent
import cats.syntax.all.toFlatMapOps
import cats.syntax.all.toFunctorOps
import cats.syntax.all.toSemigroupKOps
import cats.syntax.all.catsSyntaxApplicativeId
import cats.syntax.all.catsSyntaxSemigroup
import io.circe.generic.auto.*
import org.http4s.FormDataDecoder.formEntityDecoder
import org.http4s.HttpRoutes
import org.http4s.EntityEncoder
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.http4s.headers.Authorization
import org.typelevel.ci.CIStringSyntax
import example.project.jobsboard.core.Auth
import example.project.jobsboard.domain.Auth.LoginInfo
import example.project.jobsboard.http.responses.FailureResponse
import org.typelevel.log4cats.Logger
import org.http4s.Response
import example.project.jobsboard.domain.User
import example.project.jobsboard.domain.Auth.NewPasswordInfo
import example.project.jobsboard.domain.Aliases.AuthRoute
import tsec.authentication.asAuthed
import tsec.authentication.SecuredRequestHandler
import example.project.jobsboard.domain.Aliases.JwtToken
import tsec.authentication.TSecAuthService
import example.project.jobsboard.http.validations.validate
import example.project.jobsboard.domain.Aliases.restrictedTo
import example.project.jobsboard.domain.Aliases.adminOnly
import example.project.jobsboard.domain.Aliases.allRoles

class AuthRoutes[F[_]: Concurrent: Logger] private (auth: Auth[F]) extends Http4sDsl[F]:
  private val authenticator = auth.authenticator

  private val securedRequestHandler: SecuredRequestHandler[F, String, User, JwtToken] =
    SecuredRequestHandler(authenticator)

  // POST /auth/login json { login info } => 200 Ok with JWT as Authorization: Bearer header
  private val loginRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "login" =>
      req.validate[LoginInfo] { loginInfo =>
        for {
          token <- auth.login(loginInfo.email, loginInfo.password)
          _     <- Logger[F].info(s"User ${loginInfo.email} logged in")
          response = token.fold(
            Response[F](
              Unauthorized,
              body = EntityEncoder[F, FailureResponse].toEntity(FailureResponse("User or password is incorrect")).body
            )
          )(token => authenticator.embed(Response[F](Ok), token))
        } yield response
      }
  }

  // POST /auth/signup json { new user } => 201 Created with User
  private val signupRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "signup" =>
      println(s"res result: ${req.as[User.New]}")
      req.validate[User.New] { newUser =>
        for {
          userOpt  <- auth.signUp(newUser)
          response <- userOpt.fold(BadRequest(FailureResponse("User already exists")))(user => Created(user))
        } yield response
      }
  }

  // POST /auth/change-password json { new password info } { Authorization: Bearer } => Ok with updated user
  private val changePasswordRoute: AuthRoute[F] = {
    case secured @ POST -> Root / "change-password" asAuthed user =>
      secured.request.validate[NewPasswordInfo] { passwordInfo =>
        for {
          result <- auth.changePassword(user.email, passwordInfo)
          response <- result match
            case Right(Some(user)) => Ok(user)
            case Right(None)       => NotFound(FailureResponse(s"User ${user.email} not found"))
            case Left(error)       => Forbidden(FailureResponse(error))
        } yield response
      }
  }

  // POST /auth/logout { Authorization: Bearer } => Ok
  private val logoutRoute: AuthRoute[F] = {
    case secured @ POST -> Root / "logout" asAuthed _ =>
      for {
        _        <- Logger[F].info(s"User ${secured.authenticator.identity} logged out")
        _        <- authenticator.discard(secured.authenticator)
        response <- Ok()
      } yield response
  }

  // DELETE /auth/users/email
  private val deleteUserRoute: AuthRoute[F] = {
    case secured @ DELETE -> Root / "users" / email asAuthed user =>
      auth.delete(email).flatMap {
        case true  => Ok()
        case false => NotFound()
      }
  }

  val securedRoutes: HttpRoutes[F] = securedRequestHandler.liftService(
    changePasswordRoute.restrictedTo(allRoles) |+|
      logoutRoute.restrictedTo(allRoles) |+|
      deleteUserRoute.restrictedTo(adminOnly)
  )

  val nonSecuredRoutes: HttpRoutes[F] = loginRoute <+> signupRoute

  val routes: HttpRoutes[F] = Router[F]("/auth" -> (nonSecuredRoutes <+> securedRoutes))

object AuthRoutes:
  def make[F[_]: Concurrent: Logger](auth: Auth[F]): AuthRoutes[F] = new AuthRoutes[F](auth)
