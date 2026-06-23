package example.project.foundations

import cats.effect.kernel.MonadCancelThrow
import cats.effect.{ ExitCode, IO, IOApp }

import doobie.hikari.HikariTransactor
import doobie.implicits.*
import doobie.util.ExecutionContexts
import doobie.util.transactor.Transactor

// To run db:
// docker-compose up
// docker exec -it typelevel-project-db-1 psql -U docker
// CREATE DATABASE demo;
// \c demo
// CREATE TABLE students(id serial not null, name character varying not null, primary key(id));
// SELECT * FROM students;
// INSERT INTO students (name) VALUES ('Yoda');
// INSERT INTO students (name) VALUES ('Yaroslav');
object Dobie extends IOApp.Simple:
  final case class Student(id: Int, name: String)

  private val xa: Transactor[IO] = Transactor
    .fromDriverManager[IO]
    .apply(
      driver     = "org.postgresql.Driver",
      url        = "jdbc:postgresql:demo",
      user       = "docker",
      password   = "docker",
      logHandler = None,
    )

  def findAllStudentNames: IO[List[String]] =
    val query  = sql"SELECT name FROM students".query[String]
    val action = query.to[List]
    action.transact(xa)

  def saveStudent(s: Student): IO[Int] =
    val query  = sql"INSERT INTO students (id, name) VALUES (${s.id}, ${s.name})"
    val action = query.update.run
    action.transact(xa)

  def findStudentsByInitial(letter: String): IO[List[Student]] =
    val selectFragment = fr"SELECT id, name"
    val fromFragment   = fr"FROM students"
    val whereFragment  = fr"WHERE left(name, 1) = $letter"

    val statement = selectFragment ++ fromFragment ++ whereFragment
    val action    = statement.query[Student].to[List]

    action.transact(xa)

  // Organize code:
  // 1. Repository
  trait Students[F[_]]:
    def findById(id: Int): F[Option[Student]]
    def findAll: F[List[Student]]
    def create(name: String): F[Int]

  object Students:
    def make[F[_]: MonadCancelThrow](xa: Transactor[F]): Students[F] =
      new Students[F]:
        def findById(id: Int): F[Option[Student]] =
          sql"SELECT id, name FROM students WHERE id = $id".query[Student].option.transact(xa)

        def findAll: F[List[Student]] =
          sql"SELECT id, name FROM students".query[Student].to[List].transact(xa)

        def create(name: String): F[Int] =
          sql"INSERT INTO students (name) VALUES ($name)".update
            .withUniqueGeneratedKeys[Int]("id")
            .transact(xa)

  val postgresResources =
    for {
      ec <- ExecutionContexts.fixedThreadPool[IO](8)
      xa <- HikariTransactor.newHikariTransactor[IO](
        driverClassName = "org.postgresql.Driver",
        url             = "jdbc:postgresql:demo",
        user            = "docker",
        pass            = "docker",
        connectEC       = ec,
      )
    } yield xa

  def smallProgram = postgresResources.use { xa =>
    val studentsRepository = Students.make[IO](xa)

    for {
      id <- studentsRepository.create("Obi")
      s  <- studentsRepository.findById(id)
      _  <- IO.println(s"Create and student: $s")
    } yield ()
  }

  override def run: IO[Unit] =
    // findAllStudentNames.flatMap(IO.println)
    // saveStudent(Student(3, "Asoka")).flatMap(IO.println)
    // findStudentsByInitial("Y").flatMap(IO.println)
    smallProgram
