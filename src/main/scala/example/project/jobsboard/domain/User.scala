package example.project.jobsboard.domain

import example.project.jobsboard.domain.User.Role
import java.util.UUID

final case class User(
  id: UUID,
  email: String, // TODO: Update with Email case class
  hashedPassword: String,
  firstName: Option[String],
  lastName: Option[String],
  company: Option[String],
  role: Role,
) {
  def owns(job: Job): Boolean = job.ownerEamil.equals(email)
  def isAdmin: Boolean = role == Role.ADMIN
  def isRecruiter: Boolean = role == Role.RECRUITTER
}

object User:
  enum Role:
    case ADMIN, RECRUITTER

  final case class New private (
    email: String,
    password: String,
    firstName: Option[String],
    lastName: Option[String],
    company: Option[String],
    role: Option[Role], // TODO: fix it
  ) {
    // TODO: maybe add this on type level? NewWithHashedPassword and store only this type?
    def withHashedPassword(hashedPassword: String): New = copy(password = hashedPassword)
  }

  object New:
    def admin(
      email: String,
      password: String,
      firstName: Option[String],
      lastName: Option[String],
      company: Option[String]
    ): New =
      New(email, password, firstName, lastName, company, Some(Role.ADMIN))

    def recruiter(
      email: String,
      password: String,
      firstName: Option[String],
      lastName: Option[String],
      company: Option[String]
    ): New =
      New(email, password, firstName, lastName, company, Some(Role.RECRUITTER))
