package example.project.jobsboard.modules

import cats.effect.kernel.{ Async, Resource }
import cats.syntax.all.*

import doobie.util.transactor.Transactor
import example.project.jobsboard.core.Jobs
import example.project.jobsboard.core.Jobs.LiveJobs
import org.typelevel.log4cats.Logger
import example.project.jobsboard.core.Auth
import example.project.jobsboard.core.Users

final class Core[F[_]] private (val jobs: Jobs[F], val auth: Auth[F])

object Core:
  def apply[F[_]: Async: Logger](xa: Transactor[F]): Resource[F, Core[F]] =
    val core = for {
      jobs <- LiveJobs(xa)
      users = Users.make[F](xa)
      auth <- Auth.of(users)
    } yield new Core(jobs, auth)

    Resource.eval(core)
