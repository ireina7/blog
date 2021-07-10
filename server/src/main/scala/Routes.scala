import cats.effect.Sync
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.scalatags._
import scalatags.Text.all._
import scalatags.Text.tags2.{title => mainTitle}

// import blog.Main.content
import page.About
import java.io.File
import org.http4s._
import org.http4s.dsl._

import cats.effect._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.Server
import org.http4s.server.staticcontent._
import org.http4s.syntax.kleisli._
import scala.concurrent.ExecutionContext.global
object Routes {

    def mainRoutes[F[_]: Sync]: HttpRoutes[F] = {
        val dsl = new Http4sDsl[F]{}
        import dsl._
        HttpRoutes.of[F] {
            case req @ GET -> Root =>
                val helloWorldInScalaTags = html(mainTitle("ScalaTags Playground"), body(p("Hello World!")))
                println("!!!!!")
                Ok(helloWorldInScalaTags)
                // StaticFile.fromString("").getOrElseF(NotFound())
            case GET -> Root / "about" =>
                Ok(About.main)
            case GET -> Root / "shutdown" =>
                sys.exit()
        }
    }

    import java.util.concurrent._
    import scala.concurrent.ExecutionContext

    val blockingPool = Executors.newFixedThreadPool(4)
    val blocker = Blocker.liftExecutorService(blockingPool)
    implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

    val dsl = new Http4sDsl[IO]{}
    import dsl._
    val routes = HttpRoutes.of[IO] {
        case request @ GET -> Root =>
            StaticFile.fromFile(new File("./shared/assets/index.html"), blocker, Some(request))
                .getOrElseF(NotFound())
        case request @ GET -> Root / file if file.endsWith(".js") => 
            StaticFile.fromFile(new File(s"./shared/assets/js/$file"), blocker, Some(request))
                .getOrElseF(NotFound())
        case request @ GET -> Root / file if file.endsWith(".css") => 
            StaticFile.fromFile(new File(s"./shared/assets/css/$file"), blocker, Some(request))
                .getOrElseF(NotFound())
    }
}