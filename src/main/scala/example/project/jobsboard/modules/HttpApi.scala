package example.project.jobsboard.modules

import cats.effect.Concurrent
import cats.effect.kernel.Resource
import cats.syntax.all.toSemigroupKOps

import example.project.jobsboard.http.routes.{HealthRoutes, JobRoutes}
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.typelevel.log4cats.Logger

class HttpApi[F[_]: Concurrent: Logger] private (core: Core[F]):
  private val healthRoutes: HttpRoutes[F] = HealthRoutes[F].routes
  private val jobRoutes: HttpRoutes[F]    = JobRoutes[F](core.jobs).routes

  val routes: HttpRoutes[F] = Router("/api" -> (healthRoutes <+> jobRoutes))

object HttpApi:
  def apply[F[_]: Concurrent: Logger](core: Core[F]): Resource[F, HttpApi[F]] =
    Resource.pure(new HttpApi[F](core))
