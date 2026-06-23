package example.project.foundations

object Cats:
  // Type Classes:
  // - Applicative
  // - Functor
  // - FlatMap
  // - Monad
  // - ApplicativeError / MonadError

  // Functor - "mappable" stucture
  trait ExampleFunctor[F[_]]:
    def map[A, B](fa: F[A])(f: A => B): F[B]

  import cats.Functor
  import cats.instances.list.*
  val listFunctor = Functor[List]
  val mappedList  = listFunctor.map(List(1, 2, 3))(_ + 1)

  // Generalizable "mappable" APIs
  def increment_[F[_]](fa: F[Int])(using functor: Functor[F]): F[Int] =
    functor.map(fa)(_ + 1)

  import cats.syntax.functor.toFunctorOps
  def increment[F[_]: Functor](fa: F[Int]): F[Int] =
    fa.map(_ + 1)

  // Applicative - pure, wrap existing values into "wrapper" values
  trait ExampleApplicative[F[_]] extends ExampleFunctor[F]:
    def pure[A](value: A): F[A]

  import cats.Applicative
  val listApplicative = Applicative[List]
  val pureList        = listApplicative.pure(10)

  import cats.syntax.apply.catsSyntaxApply
  import cats.syntax.applicative.catsSyntaxApplicativeId
  val pureList10 = 10.pure[List]

  // FlatMap
  trait ExampleFlatMap[F[_]] extends ExampleFunctor[F]:
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  import cats.FlatMap
  val listFlatMap    = FlatMap[List]
  val flatMappedList = listFlatMap.flatMap(List(1, 2, 3))(n => List.fill(n)(n))

  // Monad - Applicative + FlatMap
  trait ExampleMonad[F[_]] extends ExampleApplicative[F] with ExampleFlatMap[F]:
    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)(a => pure(f(a)))

  // ApplicativeError - computations that can failed
  trait ExampleApplicativeError[F[_], E] extends ExampleApplicative[F]:
    def raiseError[A](error: E): F[A]

  import cats.ApplicativeError
  type ErrorOr[A] = Either[String, A]
  val applicativeEither          = ApplicativeError[ErrorOr, String]
  val desiredValue: ErrorOr[Int] = applicativeEither.pure(42)
  val failedValue: ErrorOr[Int]  = applicativeEither.raiseError("Something went wrong.")

  // MonadError
  trait ExampleMonadError[F[_], E] extends ExampleApplicativeError[F, E] with ExampleMonad[F]

  final case class Box[A](value: A)
  object Box:
    given Functor[Box] = new Functor[Box]:
      def map[A, B](fa: Box[A])(f: A => B): Box[B] = Box(f(fa.value))

    given Applicative[Box] = new Applicative[Box]:
      def ap[A, B](ff: Box[A => B])(fa: Box[A]): Box[B] = Box(ff.value(fa.value))
      def pure[A](x: A): Box[A] = Box(x)

    // given FlatMap[Box] = new FlatMap[Box]:
    //   def map[A, B](fa: Box[A])(f: A => B): Box[B]          = ???
    //   def flatMap[A, B](fa: Box[A])(f: A => Box[B]): Box[B] = ???

  @main def runCatsExamples(): Unit =
    // Functor
    println(increment(Box(6)))

    // Applicative
    import cats.Apply.ops.toAllApplyOps
    println(10.pure[Box])
    println(Box((x: Int) => x + 11).ap(Box(10)))

    // FlatMap
