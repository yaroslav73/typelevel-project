package example.project.foundations

import cats.effect.IOApp
import cats.effect.IO
import io.circe.generic.auto.*
import io.circe.syntax.*
import java.util.UUID
import org.http4s.dsl.impl.QueryParamDecoderMatcher
import org.http4s.dsl.impl.OptionalValidatingQueryParamDecoderMatcher
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.circe.*
import cats.Applicative
import cats.Monad
import org.http4s.ember.server.EmberServerBuilder

object Http4s extends IOApp.Simple:
  // Simulatie an HTTP server with "students" and "courses"
  type Student = String

  final case class Instructor(firstName: String, lastName: String)
  object Instructor:
    // TODO: Do not safe!
    def fromString(fullName: String): Instructor =
      val Array(firstName, lastName) = fullName.split(" ")
      Instructor(firstName, lastName)

  final case class Course(id: String, title: String, year: Int, students: List[Student], instructor: Instructor)

  object CourseRepository:
    private val catsEffectCourse = Course(
      "2f1f7e39-88e9-433d-8d45-19cac429e67a",
      "Rock the JVM Ultimate Scala Course",
      2022,
      List("Yaroslav", "Asoka"),
      Instructor("Daniel", "Ciocîrlan")
    )

    private val courses: Map[String, Course] = Map(catsEffectCourse.id -> catsEffectCourse)

    def findCourseById(courseId: UUID): Option[Course] =
      courses.get(courseId.toString)

    def findCourseByInstructor(instructor: Instructor): List[Course] =
      courses.filter((_, v) => v.instructor == instructor).values.toList

  // Essential REST endpoints
  // GET: /courses?instructor=Daniel%20Ciocîrlan&year=2022
  // GET: /courses/courseId/students

  object InstructorQueryParamMatcher extends QueryParamDecoderMatcher[String]("instructor")
  object YearQueryParamMatcher extends OptionalValidatingQueryParamDecoderMatcher[Int]("year")

  def courseRoutes[F[_]: Monad]: HttpRoutes[F] =
    val dsl = Http4sDsl[F]
    import dsl.*

    HttpRoutes.of[F] {
      case GET -> Root / "courses" :? InstructorQueryParamMatcher(instructor) +& YearQueryParamMatcher(year) =>
        val courses = CourseRepository
          .findCourseByInstructor(Instructor.fromString(instructor))
        year.fold(Ok(courses.asJson)) { validYear =>
          validYear.fold(
            _ => BadRequest("Parameter 'year' is ivalid, use year in format YYYY".asJson),
            y => Ok(courses.filter(_.year == y).asJson)
          )
        }
      case GET -> Root / "courses" / UUIDVar(courseId) / "students" =>
        CourseRepository
          .findCourseById(courseId)
          .fold {
            NotFound(s"Course $courseId not found")
          } { course =>
            Ok(course.students.asJson)
          }
    }

  // TODO: Add endpoint health, that response "All going great!"
  // TODO: Combine with courseRoutes <+> 

  override def run: IO[Unit] =
    EmberServerBuilder
      .default[IO]
      .withHttpApp(courseRoutes[IO].orNotFound)
      .build
      .use(s => IO.println(s"Searver started at: ${s.address}") *> IO.never)
