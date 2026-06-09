package example.project.jobsboard.modules

import cats.effect.kernel.{ Async, Resource }
import cats.syntax.all.*

import doobie.util.transactor.Transactor
import example.project.jobsboard.core.Jobs
import example.project.jobsboard.core.Jobs.LiveJobs
import org.typelevel.log4cats.Logger
import example.project.jobsboard.core.Auth
import example.project.jobsboard.core.Users
import example.project.jobsboard.config.SecurityConfig

final class Core[F[_]] private (val jobs: Jobs[F], val auth: Auth[F])

object Core:
  def apply[F[_]: Async: Logger](xa: Transactor[F])(securityConfig: SecurityConfig): Resource[F, Core[F]] =
    val core = for {
      jobs <- LiveJobs(xa)
      users = Users.make[F](xa)
      auth <- Auth.of(users)(securityConfig)
    } yield new Core(jobs, auth)

    Resource.eval(core)
