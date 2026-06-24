package example.project.jobsboard.http.routes

import java.time.Instant
import java.util.UUID
import scala.collection.mutable
import cats.effect.Concurrent
import cats.syntax.applicative.catsSyntaxApplicativeId
import cats.syntax.flatMap.toFlatMapOps
import cats.syntax.functor.toFunctorOps
import cats.syntax.semigroup.catsSyntaxSemigroup
import cats.syntax.semigroupk.toSemigroupKOps
import cats.{ Applicative, Monad, MonadThrow }

import example.project.jobsboard.core.Jobs
import example.project.jobsboard.domain.Job
import example.project.jobsboard.domain.Job.JobInfo
import example.project.jobsboard.http.responses.FailureResponse
import io.circe.generic.auto.*
import org.http4s.FormDataDecoder.formEntityDecoder
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.typelevel.log4cats.Logger
import example.project.jobsboard.http.validations.validate
import example.project.jobsboard.domain.Job.JobFilter
import example.project.jobsboard.utils.Pagination
import example.project.jobsboard.logging.logError
import tsec.authentication.asAuthed
import org.http4s.dsl.impl.OptionalQueryParamDecoderMatcher
import example.project.jobsboard.domain.Aliases.AuthRoute
import example.project.jobsboard.core.Auth
import tsec.authentication.SecuredRequestHandler
import example.project.jobsboard.domain.User
import example.project.jobsboard.domain.Aliases.JwtToken
import example.project.jobsboard.domain.Aliases.restrictedTo
import example.project.jobsboard.domain.Aliases.allRoles
import example.project.jobsboard.domain.Aliases.SecuredHandler
import example.project.jobsboard.domain.Aliases.Authenticator
import example.project.jobsboard.domain.User.Role

// TODO: Why we use Concurrent here?
class JobRoutes[F[_]: Concurrent: Logger] private (jobs: Jobs[F], authenticator: Authenticator[F]) extends Http4sDsl[F]:
  private val securedRequestHandler: SecuredHandler[F] = SecuredRequestHandler(authenticator)

  object LimitQueryParam extends OptionalQueryParamDecoderMatcher[Int]("limit")
  object OffsetQueryParam extends OptionalQueryParamDecoderMatcher[Int]("offset")

  // POST /jobs?limit=n&offset=k { filter }
  private val allJobsRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root :? LimitQueryParam(limit) +& OffsetQueryParam(offset) =>
      for
        filter   <- req.as[JobFilter]
        jobs     <- jobs.all(filter, Pagination(offset, limit))
        response <- Ok(jobs)
      yield response
  }

  // GET /jobs/uuid
  private val findJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / UUIDVar(id) =>
      jobs.find(id).flatMap {
        case Some(job) => Ok(job)
        case None      => NotFound(FailureResponse(s"Job with id $id not found"))
      }
  }

  // POST /jobs/new { job info }
  private val createJobRoute: AuthRoute[F] = {
    case req @ POST -> Root / "new" asAuthed _ =>
      req.request.validate[JobInfo] { jobInfo =>
        for
          id       <- jobs.create("test@test.test", jobInfo) // TODO: Remove hardcoded email
          response <- Created(id)
        yield response
      }
  }

  // PUT /jobs/uuid { job info }
  private val updateJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ PUT -> Root / UUIDVar(id) =>
      req.validate[JobInfo] { jobInfo =>
        for
          updated <- jobs.update(id, jobInfo)
          response <- updated match
            case Some(updated) => Ok(updated)
            case None          => NotFound(FailureResponse(s"Job with id $id not found"))
        yield response
      }
  }

  // DELETE /jobs/uuid
  private val deleteJobRoute: AuthRoute[F] = {
    case DELETE -> Root / UUIDVar(id) asAuthed user =>
      jobs.find(id).flatMap {
        case Some(job) if user.owns(job) || user.isAdmin =>
          for
            _        <- jobs.delete(id)
            response <- Ok(s"Job with id $id deleted")
          yield response
        case Some(job) => Forbidden(FailureResponse(s"User ${user.email} is not authorized to delete job with id $id"))
        case None      => NotFound(FailureResponse(s"Job with id $id not found"))
      }
  }

  val securedRoutes: HttpRoutes[F] = securedRequestHandler.liftService(
    createJobRoute.restrictedTo(allRoles) |+|
      deleteJobRoute.restrictedTo(allRoles)
  )

  val nonSecuredRoutes: HttpRoutes[F] = allJobsRoute <+> findJobRoute <+> updateJobRoute

  val routes: HttpRoutes[F] = Router(
    "/jobs" -> (nonSecuredRoutes <+> securedRoutes)
  )

object JobRoutes:
  def apply[F[_]: Concurrent: Logger](jobs: Jobs[F], authenticator: Authenticator[F]): JobRoutes[F] =
    new JobRoutes[F](jobs, authenticator)
