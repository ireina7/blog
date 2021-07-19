package blog.server

import cats.effect.Sync
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.scalatags.*
import scalatags.Text.all.*
import scalatags.Text.tags2.{title as mainTitle}

// import blog.Main.content
// import page.About
import java.io.File
import org.http4s.{
  client as http4sClient,
  *
}
import org.http4s.dsl.*

import cats.effect.*
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.Server
import org.http4s.server.staticcontent.*
import org.http4s.syntax.kleisli.*
import scala.concurrent.ExecutionContext.global
object Routes {

  import java.util.concurrent.*
  import scala.concurrent.ExecutionContext

  val blockingPool = Executors.newFixedThreadPool(4)
  val blocker = Blocker.liftExecutorService(blockingPool)
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  def mainRoutes[F[_]: Sync]: HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl.*
    import blog.page.*

    HttpRoutes.of[F] {

      case req @ GET -> Root =>
        val homePage = Frame.index()
        Ok(homePage)

      case req @ GET -> Root / "view" =>
        val content = Frame.index(
          div(
            for(i <- 0 to 10) yield
              Frame.item(
                title = s"第${i}篇文章！", 
                author = "Ireina", 
                date = new java.util.Date(),
                view = div(hr),
              ),
          )
        )
        Ok(content)

      case GET -> Root / "about" =>
        Ok(About.index)

      case GET -> Root / "filter" =>
        Ok(Filter.index(
          span(color := "grey")("No result")
        ))

      case GET -> Root / "shutdown" =>
        sys.exit()
    }
  }

  

  val dsl = new Http4sDsl[IO]{}
  import dsl.*
  val assets = blog.Shared.assetsPath
  val ioRoutes = HttpRoutes.of[IO] {
    // case request @ GET -> Root =>
    //     StaticFile.fromFile(new File("./shared/assets/index.html"), blocker, Some(request))
    //         .getOrElseF(NotFound())
    case request @ GET -> "assets" /: file =>
      StaticFile.fromFile(new File(s"$assets/$file"), blocker, Some(request))
        .getOrElseF(NotFound())
  }
}