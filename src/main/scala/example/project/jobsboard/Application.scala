package example.project.jobsboard

import cats.effect.{ IO, IOApp }

import example.project.jobsboard.config.{ AppConfig, loadF }
import example.project.jobsboard.modules.{ Core, HttpApi, Postgres }
import org.http4s.ember.server.EmberServerBuilder
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import pureconfig.ConfigSource
import pureconfig.error.ConfigReaderException

object Application extends IOApp.Simple:

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  private val configSource: IO[AppConfig] = ConfigSource.default.loadF[IO, AppConfig]

  def run: IO[Unit] =
    configSource.flatMap {
      case AppConfig(postgresConfig, emberConfig, securityConfig) =>
        val app =
          for
            xa      <- Postgres.make[IO](postgresConfig)
            core    <- Core[IO](xa)(securityConfig)
            httpApi <- HttpApi[IO](core, securityConfig)
            server <- EmberServerBuilder
              .default[IO]
              .withHost(emberConfig.host)
              .withPort(emberConfig.port)
              .withHttpApp(httpApi.routes.orNotFound)
              .build
          yield server

        app.use(s => IO.println(s"Server started at: ${s.address}") *> IO.never)
    }
