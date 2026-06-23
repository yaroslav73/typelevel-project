package example.project.jobsboard.http.validations

import example.project.jobsboard.domain.Job.JobInfo
import cats.data.ValidatedNel
import cats.syntax.validated.catsSyntaxValidatedId
import cats.syntax.apply.catsSyntaxTuple2Semigroupal
import cats.syntax.apply.catsSyntaxTuple5Semigroupal
import example.project.jobsboard.http.validations.Validator.ValidationFailure
import example.project.jobsboard.http.validations.Validator.ValidationResult
import java.net.URL
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import example.project.jobsboard.domain.Auth.LoginInfo
import example.project.jobsboard.domain.User
import example.project.jobsboard.domain.Auth.NewPasswordInfo

trait Validator[A]:
  def validate(value: A): ValidationResult[A]

object Validator:
  type ValidationResult[A] = ValidatedNel[ValidationFailure, A]

  sealed trait ValidationFailure(val error: String)
  final case class EmptyField(fieldName: String) extends ValidationFailure(s"$fieldName is empty")
  final case class InvalidUrl(fieldName: String) extends ValidationFailure(s"$fieldName invalid URL")
  final case class InvalidEmail(fieldName: String) extends ValidationFailure(s"$fieldName invalid email")

  private def validateRequired[A](value: A, fieldName: String)(required: A => Boolean): ValidationResult[A] =
    if required(value) then value.validNel
    else EmptyField(fieldName).invalidNel

  private def validateUrl(value: String, fieldName: String): ValidationResult[String] =
    Try(URL(value).toURI()) match
      case Success(_) => value.validNel
      case Failure(_) => InvalidUrl(fieldName).invalidNel

  private def validateEmail(email: String, fieldName: String): ValidationResult[String] =
    val emailRegex =
      """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r
    emailRegex.findFirstMatchIn(email) match
      case Some(_) => email.validNel
      case None    => InvalidEmail(fieldName).invalidNel

  given jobInfoValidator: Validator[JobInfo] = (jobInfo: JobInfo) => {
    val JobInfo(
      company,
      title,
      description,
      externalUrl,
      location,
      remote,
      salary,
      currency,
      country,
      tags,
      image,
      seniority,
      other,
    ) = jobInfo

    val validCompany     = validateRequired(company, "company")(_.nonEmpty)
    val validTitle       = validateRequired(title, "title")(_.nonEmpty)
    val validDescription = validateRequired(description, "description")(_.nonEmpty)
    val validExternalUrl = validateUrl(externalUrl, "externalUrl")
    val validLocation    = validateRequired(location, "location")(_.nonEmpty)

    (validCompany, validTitle, validDescription, validExternalUrl, validLocation).mapN(
      (company, title, description, externalUrl, location) =>
        JobInfo(
          company,
          title,
          description,
          externalUrl,
          location,
          remote,
          salary,
          currency,
          country,
          tags,
          image,
          seniority,
          other,
        )
    )
  }

  given loginInfoValidator: Validator[LoginInfo] = (loginInfo: LoginInfo) => {
    val validEmail    = validateRequired(loginInfo.email, "email")(_.nonEmpty).andThen(e => validateEmail(e, "email"))
    val validPassword = validateRequired(loginInfo.password, "password")(_.nonEmpty)

    (validEmail, validPassword).mapN((email, password) => LoginInfo(email, password))
  }

  given newUserValidator: Validator[User.New] = (newUser: User.New) => {
    println(s"Validating new user: $newUser")
    val User.New(email, password, firstName, lastName, company, role) = newUser

    val validEmail    = validateRequired(email, "email")(_.nonEmpty).andThen(e => validateEmail(e, "email"))
    val validPassword = validateRequired(password, "password")(_.nonEmpty)
    // Password validation here

    (validEmail, validPassword).mapN { (email, password) =>
      User.New.recruiter(email, password, firstName, lastName, company)
      // role match
      //   case User.Role.ADMIN      => User.New.admin(email, password, firstName, lastName, company)
      //   case User.Role.RECRUITTER => User.New.recruiter(email, password, firstName, lastName, company)
    }
  }

  given newPasswordInfo: Validator[NewPasswordInfo] = (newPasswordInfo: NewPasswordInfo) => {
    val NewPasswordInfo(oldPassword, newPassword) = newPasswordInfo

    val validPassword    = validateRequired(oldPassword, "old password")(_.nonEmpty)
    val validNewPassword = validateRequired(newPassword, "new password")(_.nonEmpty)

    (validPassword, validNewPassword).mapN((oldPassword, newPassword) => NewPasswordInfo(oldPassword, newPassword))
  }
