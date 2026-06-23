package example.project.jobsboard.playground

import scala.io.StdIn
import cats.effect.implicits.*
import cats.effect.kernel.Resource
import cats.effect.{ IO, IOApp }
import cats.syntax.all.*

import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts
import example.project.jobsboard.core.Jobs.LiveJobs
import example.project.jobsboard.domain.Job.JobInfo
import example.project.jobsboard.domain.Job.JobFilter
import example.project.jobsboard.utils.Pagination
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

// TODO: Move it to the tests
object JobsPlayground extends IOApp.Simple:

  val postrgresResource: Resource[IO, HikariTransactor[IO]] =
    for
      ec <- ExecutionContexts.fixedThreadPool[IO](32)
      xa <- HikariTransactor.newHikariTransactor[IO](
        "org.postgresql.Driver",
        "jdbc:postgresql:board",
        "docker",
        "docker",
        ec,
      )
    yield xa

  val jobInfo = JobInfo.Test

  override def run: IO[Unit] =
    given Logger[IO] = Slf4jLogger.getLogger[IO]

    postrgresResource.use { xa =>
      for
        jobs <- LiveJobs[IO](xa)
        _    <- IO.println("Ready. Next...") *> IO(StdIn.readLine())
        id   <- jobs.create("test@test.test", jobInfo)
        _    <- IO.println("Next...") *> IO(StdIn.readLine())
        list <- jobs.all(JobFilter(), Pagination.Default)
        _    <- IO.println(s"All jobs: $list. \nNext...") *> IO(StdIn.readLine())
        job  <- jobs.update(id, jobInfo.copy(title = "Software Engineer II"))
        _    <- IO.println(s"Updated job: $job, \nNext...") *> IO(StdIn.readLine())
        _    <- jobs.delete(id)
        list <- jobs.all(JobFilter(), Pagination.Default)
        _    <- IO.println(s"Job deleted: $list. \nPress enter to end.") *> IO(StdIn.readLine())
      yield ()
    }
