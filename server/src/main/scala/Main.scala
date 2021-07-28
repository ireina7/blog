package blog.server

// import cats.effect.{ ExitCode, IO, IOApp }
import cats.effect.*
import cats.implicits.*
import org.http4s.server.staticcontent.*
import org.http4s.syntax.kleisli.*
import org.http4s.server.Server
import org.http4s.server.blaze.BlazeServerBuilder
import scala.concurrent.ExecutionContext.global

// object Main extends IOApp {
//     def run(args: List[String]) =
//         Server.stream[IO].compile.drain.as(ExitCode.Success)
// }

object BlogHttpServer extends IOApp {
  
  override def run(args: List[String]): IO[ExitCode] =
    app.use(_ => IO.never).as(ExitCode.Success)

  val app: Resource[IO, Server[IO]] =
    for {
      blocker <- Blocker[IO]
      server  <- BlazeServerBuilder[IO](global)
        .bindHttp(8080)
        .withHttpApp((Routes.mainRoutes[IO] <+> Routes.ioRoutes).orNotFound)
        .resource
    } yield server
}