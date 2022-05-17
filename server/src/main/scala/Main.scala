package blog.server

// import cats.effect.{ ExitCode, IO, IOApp }
import cats.effect.*
import cats.implicits.*
import org.http4s.server.staticcontent.*
import org.http4s.syntax.kleisli.*
import org.http4s.server.Server
// import org.http4s.HttpServer
import org.http4s.blaze.server.BlazeServerBuilder
import scala.concurrent.ExecutionContext.global
// import specs2.run
// import org.http4s.HttpApp
// import org.http4s.server.Server



// object JavaServer:

//   def main(args: Array[String]): Unit = {
//     import blog.MioServer
//     val server = new MioServer()
//     server.serve()
//   }

// end JavaServer


object Application extends IOApp:

  override def run(args: List[String]): IO[ExitCode] =
    BlogHttpServer
      .app.use(_ => IO.never).as(ExitCode.Success)


  // def runSuperServer(args: List[String]): IO[ExitCode] =
  //   SuperBlogHttpServer
  //     .app.use(???).as(ExitCode.Success)

  extension (io: IO.type)
    def println[A](a: A): IO[Unit] = IO(scala.Predef.println(a))
  
  def test: IO[Unit] = 
    IO.println("This is a effect testing...")
      >> IO.println(Thread.currentThread)
      >> IO.shift
      >> IO.println(Thread.currentThread)
      >> IO.println("End test.")
    
  end test

end Application



object BlogHttpServer {
  
  import blog.core.Effect.*
  import blog.core.Effect.given
  // override def run(args: List[String]): IO[ExitCode] =
  //   app.use(_ => IO.never).as(ExitCode.Success)
  given cs: ContextShift[IO] = IO.contextShift(global)
  given Timer[IO] = IO.timer(global)
  val app: ConcurrentEffect[IO] ?=> Resource[IO, Server] =
    for {
      blocker <- Blocker[IO]
      server  <- BlazeServerBuilder.apply[IO](global)
        .bindHttp(8080)
        .withHttpApp((Routes.mainRoutes[IO] <+> Routes.ioRoutes).orNotFound)
        .resource
    } yield server
}


/** Experimental super http server with super computation effect
 * - underworking!
*/
object SuperBlogHttpServer:
  import blog.core.Effect.{*, given}
  import scala.concurrent.ExecutionContext
  import scala.concurrent.duration.*
  import cats.effect.Clock
  import cats.data.EitherT
  import scala.concurrent.duration.FiniteDuration

  type Eff[A] = Injection[IOErr, blog.Configuration, A]

  val ioCS: ContextShift[IO] = IO.contextShift(global)
  val ioTimer: Timer[IO] = IO.timer(global)
  given blog.Configuration = blog.Configuration.onlineBlog
  given ContextShift[Eff] with
    def shift: Eff[Unit] = conf ?=> 
      EitherT.right(ioCS.shift)
    def evalOn[A](ec: ExecutionContext)(f: Eff[A]): Eff[A] = conf ?=>
      val ioF: IO[A] = f(using conf).value.flatMap {
        case Right(x) => IO(x)
        case Left(er) => IO.raiseError(er)
      }
      EitherT.right(ioCS.evalOn(ec)(ioF))
  
  given Timer[Eff] with
    def clock: Clock[Eff] = new Clock {
      override def realTime(unit: TimeUnit): Eff[Long] =
        conf ?=> EitherT.right(ioTimer.clock.realTime(unit))

      override def monotonic(unit: TimeUnit): Eff[Long] =
        conf ?=> EitherT.right(ioTimer.clock.monotonic(unit))
    }
    def sleep(duration: FiniteDuration): Eff[Unit] = conf ?=>
      EitherT.right(ioTimer.sleep(duration))

  
  val app: ConcurrentEffect[Eff] ?=> Resource[Eff, Server] =
    for 
      blocker <- Blocker[Eff]
      server  <- BlazeServerBuilder.apply[Eff](global)
        .bindHttp(8080)
        .withHttpApp((Routes.mainRoutes[Eff]).orNotFound)
        .resource
    yield server
  
end SuperBlogHttpServer






// object Main extends IOApp {
//     def run(args: List[String]) =
//         Server.stream[IO].compile.drain.as(ExitCode.Success)
// }

// def naiveEffectManagement
//   [ F[_]: Monad, 
//     Program1[_], Program2[_], 
//     Cap1, Cap2, 
//     Input1, Input2,
//     Output1, Output2,
//     Input, Output,
//   ]
//   (input: Input)
//   (using program1: Program1[[A] =>> Cap1 ?=> F[A], Input1, Output1])
//   (using program2: Program2[[A] =>> Cap2 ?=> F[A], Input2, Output2])
//   : (Cap1, Cap2) ?=> F[Output] = {
  
//   for
//     x <- program1.run(input)
//     y <- program2.run(x)
//   yield y
// }



