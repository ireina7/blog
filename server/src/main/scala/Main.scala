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
import specs2.run
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
  
  // override def run(args: List[String]): IO[ExitCode] =
  //   app.use(_ => IO.never).as(ExitCode.Success)
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



