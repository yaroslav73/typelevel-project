package example.project.jobsboard.core

import cats.syntax.all.*
import tsec.authentication.AugmentedJWT
import tsec.mac.jca.HMACSHA256
import example.project.jobsboard.domain.Auth.NewPasswordInfo
import example.project.jobsboard.domain.User
import example.project.jobsboard.domain.User.New
import example.project.jobsboard.domain.Aliases.JwtToken
import example.project.jobsboard.domain.Aliases.Authenticator
import cats.Applicative
import tsec.passwordhashers.jca.BCrypt
import tsec.passwordhashers.PasswordHash
import cats.effect.kernel.Sync
import tsec.authentication.JWTAuthenticator
import concurrent.duration.DurationInt
import tsec.authentication.IdentityStore
import cats.data.OptionT
import tsec.authentication.BackingStore
import tsec.common.SecureRandomId
import cats.effect.kernel.Ref

trait Auth[F[_]]:
  def login(email: String, password: String): F[Option[JwtToken]]
  def signUp(user: User.New): F[Option[User]]
  def delete(email: String): F[Boolean]
  def changePassword(email: String, newPassword: NewPasswordInfo): F[Either[String, Option[User]]]
  def authenticator: Authenticator[F]

object Auth:
  def make[F[_]: Sync](users: Users[F], auth: Authenticator[F]): Auth[F] = new Auth[F] {
    def login(email: String, password: String): F[Option[JwtToken]] =
      for {
        user  <- users.find(email)
        user  <- user.filterA(user => checkPassword(password, user.hashedPassword))
        token <- user.traverse(user => authenticator.create(user.email))
      } yield token

    def signUp(user: New): F[Option[User]] =
      for {
        userOpt <- users.find(user.email)
        user <- userOpt match
          case Some(_) => None.pure[F]
          case _ =>
            for {
              hashedPassword <- BCrypt.hashpw[F](user.password)
              userId         <- users.create(user.withHashedPassword(hashedPassword))
              user           <- users.find(userId)
            } yield user
      } yield user

    def delete(email: String): F[Boolean] =
      for {
        userOpt <- users.find(email)
        result <- userOpt match
          case Some(user) => users.delete(user.id)
          case None       => false.pure[F]
      } yield result

    def changePassword(email: String, passwordInfo: NewPasswordInfo): F[Either[String, Option[User]]] =
      for {
        user <- users.find(email)
        user <- user match
          case Some(user) =>
            checkPassword(passwordInfo.oldPassword, user.hashedPassword).ifM(
              updatePassword(user, passwordInfo.newPassword).map(_.asRight),
              "Invalid password".asLeft.pure[F]
            )
          case None => "User with this email not found".asLeft.pure[F]
      } yield user

    def authenticator: Authenticator[F] = auth

    private def updatePassword(user: User, newPassword: String): F[Option[User]] =
      for {
        hashedPassword <- BCrypt.hashpw[F](newPassword)
        user           <- users.update(user.copy(hashedPassword = hashedPassword))
      } yield user

    private def checkPassword(password: String, hashedPassword: String): F[Boolean] =
      BCrypt.checkpwBool[F](password, PasswordHash[BCrypt](hashedPassword))
  }

  def of[F[_]: Sync](users: Users[F]): F[Auth[F]] = {
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
    val keyF = HMACSHA256.buildKey[F]("secret-key".getBytes("UTF-8")) // TODO: extract secret key to config

    // 4. Authenticator and 5. Auth
    for {
      key        <- keyF
      tokenStore <- tokenStoreF
      authenticator = JWTAuthenticator.backed.inBearerToken(
        expiryDuration = 1.day, // TODO: extract to config?
        maxIdle        = None,
        tokenStore     = tokenStore,
        identityStore  = idStore,
        signingKey     = key
      )
    } yield make(users, authenticator)
  }
