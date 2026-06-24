package example.project.jobsboard.core

import cats.effect.kernel.MonadCancelThrow
import cats.syntax.*
import cats.implicits.*

import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.util.*
import example.project.jobsboard.domain.User
import doobie.util.transactor.Transactor
import java.util.UUID

trait Users[F[_]]:
  def find(email: String): F[Option[User]]
  def find(id: UUID): F[Option[User]]
  def create(user: User.New): F[UUID]
  def update(user: User): F[Option[User]]
  def delete(id: UUID): F[Boolean]

object Users:
  def of[F[_]: MonadCancelThrow](xa: Transactor[F]): Users[F] = new Users[F] {
    def find(email: String): F[Option[User]] =
      sql"SELECT * FROM users WHERE email = $email"
        .query[User]
        .option
        .transact(xa)

    def find(id: UUID): F[Option[User]] =
      sql"SELECT * FROM users WHERE id = $id"
        .query[User]
        .option
        .transact(xa)

    def create(user: User.New): F[UUID] =
      sql"""
        INSERT INTO users (email, password, first_name, last_name, company, role)
        VALUES (${user.email}, ${user.password}, ${user.firstName}, ${user.lastName}, ${user.company}, ${user.role})
      """.update
        .withUniqueGeneratedKeys[UUID]("id")
        .transact(xa)

    def update(user: User): F[Option[User]] =
      sql"""
        UPDATE users
        SET 
          email = ${user.email}, 
          password = ${user.hashedPassword}, 
          first_name = ${user.firstName}, 
          last_name = ${user.lastName}, 
          company = ${user.company}, 
          role = ${user.role}
        WHERE id = ${user.id}
      """.update.run
        .transact(xa)
        .flatMap(_ => find(user.id))

    def delete(id: UUID): F[Boolean] =
      sql"DELETE FROM users WHERE id = $id".update.run
        .transact(xa)
        .map(_ > 0)
  }

  // TODO: Extract to the separate layer - meta
  given metaRole: Meta[User.Role] = Meta[String].timap[User.Role](User.Role.valueOf)(_.toString)
