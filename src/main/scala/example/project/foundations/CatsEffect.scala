package example.project.foundations

import java.io.{ File, FileWriter, PrintWriter }
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{ DurationInt, FiniteDuration }
import scala.io.{ Source, StdIn }
import scala.util.Random
import cats.effect.kernel.{ Concurrent, Deferred, Fiber, GenSpawn, MonadCancel, Ref, Resource, Spawn, Sync, Temporal }
import cats.effect.{ IO, IOApp }
import cats.{ Defer, MonadError }

object CatsEffect extends IOApp.Simple:
  // Describing computations as values

  // IO - data structure describing arbitrary computations (including side effects)
  val firstIO: IO[Int] = IO.pure(73)
  val delayedIO: IO[Int] = IO {
    println("Hello, from IO")
    73
  }

  def evaluateIO[A](io: IO[A]): Unit =
    import cats.effect.unsafe.implicits.global
    val result = io.unsafeRunSync()
    println(s"The result of the effect is: $result")

  // Transformations
  // map + flatMap
  val printedFirstIO = firstIO.flatMap(n => IO(println(n)))

  // For-comprehension
  def smallProgram(): IO[Unit] =
    for {
      line1 <- IO(StdIn.readLine("Enter some text:"))
      line2 <- IO(StdIn.readLine("Enter another text:"))
      _     <- IO(println(s"$line1 and $line2"))
    } yield ()

  // Raise / handle (catch) errors
  val successIO: IO[Int] = IO(13)
  val failureIO: IO[Int] = IO.raiseError(new RuntimeException("a proper failure"))
  val handledIO: IO[Int] = failureIO.handleErrorWith {
    case e: RuntimeException =>
      IO(println(s"Handle error with message: ${e.getMessage}")) *> IO(0)
  }

  // Fibers - "lightweight threads"
  val delayedPrint = IO.sleep(1.second) *> IO(println(s"[${Thread.currentThread.getName}]: ${Random.nextInt(100)}"))
  val manyPrintsV1 =
    for {
      _ <- delayedPrint
      _ <- delayedPrint
    } yield ()

  val manyPrintsV2 =
    for {
      _ <- delayedPrint.start
      _ <- delayedPrint
    } yield ()

  val manyPrintsV3 =
    for {
      fiber1 <- delayedPrint.start
      fiber2 <- delayedPrint.start
      _      <- fiber1.join
      _      <- fiber2.join
    } yield ()

  val cancelledFiber =
    for {
      fiber <- delayedPrint.onCancel(IO(println("I'm cancelled"))).start
      _     <- IO.sleep(500.millis) *> IO(println("Cancelling fiber")) *> fiber.cancel
      _     <- fiber.join
    } yield ()

  // Uncancellation
  val ignoredCancellation =
    for {
      fiber <- IO.uncancelable(_ => delayedPrint.onCancel(IO(println("I'm cancelled")))).start
      _     <- IO.sleep(500.millis) *> IO(println("Cancelling fiber")) *> fiber.cancel
      _     <- fiber.join
    } yield ()

  // Resources
  val readingResource =
    Resource.make(
      IO.println("Acquring resources...") *>
        IO(Source.fromFile("src/main/scala/example/project/foundations/CatsEffect.scala"))
    )(source => IO.println("Closing source...") *> IO(source.close))

  val readingEffect = readingResource.use(source => IO(source.getLines().foreach(println)))

  // Compose resources
  val copiedFileResource = Resource.make(
    IO.println("Acquring dumped file...") *>
      IO(new PrintWriter(new FileWriter(new File("src/main/resources/dupm_file.scala"))))
  )(writer => IO.println("Closing dumped file...") *> IO(writer.close))

  val compositeResource =
    for {
      source      <- readingResource
      destination <- copiedFileResource
    } yield (source, destination)

  val copyFileEffect = compositeResource.use {
    case (source, destination) =>
      IO(source.getLines().foreach(destination.println))
  }

  // Abstract kinds of computations

  // MonadCancel - cancellable computations
  trait MonadCancelExample[F[_], E] extends MonadError[F, E]:
    trait CancellationFlatResetter:
      def apply[A](fa: F[A]): F[A] // With the cancellation flag reset

    def canceled: F[Unit]
    def uncancelable[A](poll: CancellationFlatResetter => F[A]): F[A]

  // MonadCancel for IO
  val monadCancel: MonadCancel[IO, Throwable] = MonadCancel[IO]
  val uncancelableIO = monadCancel.uncancelable(_ => IO(73)) // The same as IO.uncancelable(...)

  // Spawn - ability to create fibers
  trait GenSpawnExample[F[_], E] extends MonadCancel[F, E]:
    def start[A](fa: F[A]): F[Fiber[F, E, A]] // Craetes a fiber
    // ...
    // never, cede, racePair

  trait SpawnExample[F[_], E] extends GenSpawn[F, Throwable]

  val spawnIO = Spawn[IO]
  val fiber   = spawnIO.start(delayedPrint) // Creates a fiber, the same as delayedPrint.start

  // Concurrent - concurrency primitives (atomic references + promises)
  trait ConcurrentExample[F[_]] extends Spawn[F]:
    def ref[A](a: A): F[Ref[F, A]]
    def deferred[A]: F[Deferred[F, A]]

  // Temporal - ability to suspend computations for a given time
  trait TemporalExample[F[_]] extends Concurrent[F]:
    def sleep(time: FiniteDuration): F[Unit]

  // Sync - ability to suspend synchronous arbitrary expressions in an Effect
  trait SyncExample[F[_]] extends MonadCancel[F, Throwable] with Defer[F]:
    def delay[A](expression: => A): F[A]
    def blocking[A](expression: => A): F[A] // Runs on a dedicated blocking thread pool

  // Async - ability to suspend asynchronous computations (i.e. on other thread pools) into an Effect managed by CE
  trait AsyncExample[F[_]] extends Sync[F] with Temporal[F]:
    def executionContext: F[ExecutionContext]
    def async[A](callback: (Either[Throwable, A] => Unit) => F[Option[F[Unit]]]): F[A]

  def run: IO[Unit] = copyFileEffect
