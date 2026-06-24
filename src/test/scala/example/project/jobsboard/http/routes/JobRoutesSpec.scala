package example.project.jobsboard.http.routes

import java.util.UUID
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec

import example.project.jobsboard.core.Jobs
import example.project.jobsboard.domain.Job
import example.project.jobsboard.domain.Job.JobInfo
import example.project.jobsboard.fixtures.{ JobFixture, UserFixture }
import example.project.jobsboard.http.responses.FailureResponse
import io.circe.Json
import io.circe.generic.auto.*
import org.http4s.*
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.dsl.*
import org.http4s.dsl.io.*
import org.http4s.implicits.*
import org.http4s.server.Router
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import example.project.jobsboard.utils.Pagination
import example.project.jobsboard.domain.Job.JobFilter
import example.project.jobsboard.stubs.AuthenticatorStub
import example.project.jobsboard.domain.Aliases.JwtToken
import tsec.jws.mac.JWTMac
import tsec.mac.jca.HMACSHA256
import org.http4s.headers.Authorization

class JobRoutesSpec
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with Http4sDsl[IO]
    with JobFixture
    with UserFixture:

  "JobRoutes" - {
    "should return a job with given id" in {
      val request = Request[IO](Method.GET, uri"/jobs" / TestJobId)

      for
        response <- jobRoutes.run(request)
        payload  <- response.as[Job]
      yield
        response.status shouldBe Status.Ok
        payload         shouldBe TestJob
    }

    "should return all jobs" in {
      val request = Request[IO](Method.POST, uri"/jobs").withEntity(JobFilter())

      for
        response <- jobRoutes.run(request)
        payload  <- response.as[List[Job]]
      yield
        response.status shouldBe Status.Ok
        payload         shouldBe List(TestJob)
    }

    "should return all remote jobs" in {
      val request = Request[IO](Method.POST, uri"/jobs").withEntity(JobFilter(remote = true))

      for
        response <- jobRoutes.run(request)
        payload  <- response.as[List[Job]]
      yield
        response.status shouldBe Status.Ok
        payload         shouldBe List.empty
    }

    "should create a new job" in {
      val request = Request[IO](Method.POST, uri"/jobs/new").withEntity(NewTestJobInfo)

      for
        jwtToken <- authenticator.create(john.email)
        response <- jobRoutes.run(request.withBearerToken(jwtToken))
        payload  <- response.as[UUID]
      yield
        response.status shouldBe Status.Created
        payload         shouldBe NewTestJobId
    }

    "should update a job" in {
      val request = Request[IO](Method.PUT, uri"/jobs" / TestJobId).withEntity(UpdateTestJobInfo)

      for
        response <- jobRoutes.run(request)
        payload  <- response.as[Option[Job]]
      yield
        response.status shouldBe Status.Ok
        payload         shouldBe Some(UpdatedTestJob)
    }

    "should with update a job with wrong id" in {
      val request = Request[IO](Method.PUT, uri"/jobs" / NotFoundJobId).withEntity(UpdateTestJobInfo)

      for
        response <- jobRoutes.run(request)
        payload  <- response.as[FailureResponse]
      yield
        response.status shouldBe Status.NotFound
        payload         shouldBe FailureResponse(s"Job with id $NotFoundJobId not found")
    }

    "should delete a job" in {
      val request = Request[IO](Method.DELETE, uri"/jobs" / TestJobId)

      for
        jwtToken <- authenticator.create(john.email)
        response <- jobRoutes.run(request.withBearerToken(jwtToken))
        payload  <- response.as[String]
      yield
        response.status shouldBe Status.Ok
        payload         shouldBe "Job with id 843df718-ec6e-4d49-9289-f799c0f40064 deleted"
    }

    "should not delete a job with wrong id" in {
      val request = Request[IO](Method.DELETE, uri"/jobs" / NotFoundJobId)

      for
        jwtToken <- authenticator.create(john.email)
        response <- jobRoutes.run(request.withBearerToken(jwtToken))
        payload  <- response.as[FailureResponse]
      yield
        response.status shouldBe Status.NotFound
        payload         shouldBe FailureResponse(s"Job with id $NotFoundJobId not found")
    }

    "should not delete a job if user is not authorized" in {
      val request = Request[IO](Method.DELETE, uri"/jobs" / TestJobId)

      for
        jwtToken <- authenticator.create(anna.email)
        response <- jobRoutes.run(request.withBearerToken(jwtToken))
        payload  <- response.as[FailureResponse]
      yield
        response.status shouldBe Status.Forbidden
        payload shouldBe FailureResponse(s"User ${anna.email} is not authorized to delete job with id $TestJobId")
    }
  }

  private val jobs = new Jobs[IO] {
    override def all(): IO[List[Job]] =
      IO.pure(List(TestJob))

    override def all(filter: JobFilter, pagination: Pagination): IO[List[Job]] =
      if filter.remote then IO.pure(List.empty[Job])
      else IO.pure(List(TestJob))

    override def find(id: UUID): IO[Option[Job]] =
      all().map(_.find(_.id == id))

    override def create(email: String, jobInfo: JobInfo): IO[UUID] =
      IO.pure(NewTestJobId)

    override def update(id: UUID, jobInfo: JobInfo): IO[Option[Job]] =
      IO.pure(Option.when(id == TestJobId)(UpdatedTestJob))

    override def delete(id: UUID): IO[Int] =
      if id == TestJobId then IO.pure(1) else IO.pure(0)
  }

  private val authenticator = AuthenticatorStub[IO]()

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]
  private val jobRoutes = JobRoutes[IO](jobs, authenticator).routes.orNotFound

  extension (request: Request[IO])
    def withBearerToken(token: JwtToken): Request[IO] =
      request.putHeaders {
        val jwtString = JWTMac.toEncodedString[IO, HMACSHA256](token.jwt)
        Authorization(Credentials.Token(AuthScheme.Bearer, jwtString))
      }
