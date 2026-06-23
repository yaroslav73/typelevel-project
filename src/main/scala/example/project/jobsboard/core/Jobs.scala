package example.project.jobsboard.core

import java.time.Instant
import java.util.UUID
import cats.effect.kernel.MonadCancelThrow
import cats.syntax.*
import cats.implicits.*

import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.util.*
import example.project.jobsboard.domain.Job
import example.project.jobsboard.domain.Job.JobInfo
import example.project.jobsboard.domain.Job.JobFilter
import example.project.jobsboard.utils.Pagination
import doobie.util.fragment.Fragment
import org.typelevel.log4cats.Logger
import example.project.jobsboard.logging.logError

trait Jobs[F[_]]:
  def all(): F[List[Job]]
  def all(filter: JobFilter, pagination: Pagination): F[List[Job]]
  def create(ownerEmail: String, job: JobInfo): F[UUID]
  def find(id: UUID): F[Option[Job]]
  def update(id: UUID, jobInfo: JobInfo): F[Option[Job]]
  def delete(id: UUID): F[Int]

object Jobs:
  class LiveJobs[F[_]: MonadCancelThrow: Logger] private (xa: Transactor[F]) extends Jobs[F]:
    def all(): F[List[Job]] =
      sql"""
        SELECT 
          id, 
          timestamp, 
          owner_email, 
          company, 
          title, 
          description, 
          external_url, 
          location, 
          remote, 
          salary, 
          currency, 
          country, 
          tags, 
          image, 
          seniority, 
          other, 
          active
        FROM jobs
      """
        .query[Job]
        .to[List]
        .transact(xa)

    def all(filter: JobFilter, pagination: Pagination): F[List[Job]] = {
      val selectFr: Fragment =
        fr"""
          SELECT 
            id, 
            timestamp, 
            owner_email, 
            company, 
            title, 
            description, 
            external_url, 
            location, 
            remote, 
            salary, 
            currency, 
            country, 
            tags, 
            image, 
            seniority, 
            other, 
            active
        """

      val fromFr: Fragment = fr"FROM jobs"

      val whereFr: Fragment = {
        val companies = filter.companies.toNel.map(c => Fragments.in(fr"company", c))
        val locations = filter.locations.toNel.map(l => Fragments.in(fr"location", l))
        val countries = filter.countries.toNel.map(c => Fragments.in(fr"country", c))
        val tags      = filter.tags.toNel.flatMap(tags => Fragments.orOpt(tags.toList.map(tag => fr"$tag=ANY(tags)")))
        val seniority = filter.seniority.map(s => fr"seniority = $s")
        val salary    = filter.salary.map(s => fr"salary >= $s")
        val remote    = Option(fr"remote = ${filter.remote}")

        Fragments.whereAndOpt(companies, locations, countries, tags, seniority, salary, remote)
      }

      val paginationFr: Fragment = fr"ORDER BY id LIMIT ${pagination.limit} OFFSET ${pagination.offset}"

      val statement = (selectFr ++ fromFr ++ whereFr ++ paginationFr)

      Logger[F].info(s"Query: ${statement.toString}") *>
        statement
          .query[Job]
          .to[List]
          .transact(xa)
          .logError(e => s"Error fetching jobs: ${e.getMessage}")
    }

    def create(ownerEmail: String, jobInfo: JobInfo): F[UUID] =
      sql"""
        INSERT INTO jobs ( 
          timestamp, 
          owner_email, 
          company, 
          title, 
          description, 
          external_url, 
          location, 
          remote, 
          salary, 
          currency, 
          country, 
          tags, 
          image, 
          seniority, 
          other, 
          active
        ) VALUES (
          ${Instant.now()}, 
          $ownerEmail, 
          ${jobInfo.company}, 
          ${jobInfo.title}, 
          ${jobInfo.description}, 
          ${jobInfo.externalUrl}, 
          ${jobInfo.location}, 
          ${jobInfo.remote}, 
          ${jobInfo.salary}, 
          ${jobInfo.currency}, 
          ${jobInfo.country}, 
          ${jobInfo.tags}, 
          ${jobInfo.image}, 
          ${jobInfo.seniority}, 
          ${jobInfo.other}, 
          false
        )
      """.update
        .withUniqueGeneratedKeys[UUID]("id")
        .transact(xa)

    def find(id: UUID): F[Option[Job]] =
      sql"""
        SELECT 
          id, 
          timestamp, 
          owner_email, 
          company, 
          title, 
          description, 
          external_url, 
          location, 
          remote, 
          salary, 
          currency, 
          country, 
          tags, 
          image, 
          seniority, 
          other, 
          active
        FROM jobs
        WHERE id = $id
      """
        .query[Job]
        .option
        .transact(xa)

    def update(id: UUID, jobInfo: JobInfo): F[Option[Job]] =
      sql"""
        UPDATE jobs
        SET 
          company = ${jobInfo.company}, 
          title = ${jobInfo.title}, 
          description = ${jobInfo.description}, 
          external_url = ${jobInfo.externalUrl}, 
          location = ${jobInfo.location}, 
          remote = ${jobInfo.remote}, 
          salary = ${jobInfo.salary}, 
          currency = ${jobInfo.currency}, 
          country = ${jobInfo.country}, 
          tags = ${jobInfo.tags}, 
          image = ${jobInfo.image}, 
          seniority = ${jobInfo.seniority}, 
          other = ${jobInfo.other}
        WHERE id = $id
      """.update.run
        .transact(xa)
        .flatMap(_ => find(id))

    def delete(id: UUID): F[Int] =
      sql"""
        DELETE FROM jobs
        WHERE id = $id
      """.update.run
        .transact(xa)

  object LiveJobs:
    // TODO: Do we need read for the Job?
    def apply[F[_]: MonadCancelThrow: Logger](xa: Transactor[F]): F[LiveJobs[F]] = new LiveJobs[F](xa).pure[F]
