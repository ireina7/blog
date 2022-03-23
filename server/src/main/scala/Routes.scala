package blog.server


import cats.effect.*

import scalatags.Text.all.*
import scalatags.Text.tags2.{title as mainTitle}
import java.io.File

import org.http4s
import http4s.HttpRoutes
import http4s.dsl.Http4sDsl
import http4s.scalatags.*
import http4s.{
  client as http4sClient,
  *
}
import http4s.dsl.*
import http4s.server
import http4s.syntax.kleisli.*
import server.blaze.BlazeServerBuilder
import server.Server
import server.staticcontent.*
import scala.concurrent.ExecutionContext


object Routes {

  given blog.Configuration = blog.Configuration.onlineBlog

  def mainRoutes[F[_]: Sync]: HttpRoutes[F] =
    val dsl = new Http4sDsl[F]{}
    import dsl.*
    import blog.page.*

    HttpRoutes.of[F] {

      case GET -> Root =>
        val homePage = Frame.index()
        Ok(homePage)

      case GET -> Root / "view" =>
        val content = Frame.index(
          div(
            for(i <- 0 to 10) yield
              Frame.item(blog.page.Item(
                title = s"第${i}篇文章！", 
                link = "???",
                author = "Ireina", 
                date = java.util.Date().toString,
                view = "")
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
  end mainRoutes

  
  import java.util.concurrent.*
  val blockingPool = Executors.newFixedThreadPool(4)
  val blocker = Blocker.liftExecutorService(blockingPool)
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val dsl = new Http4sDsl[IO]{}
  import dsl.*
  val assets = blog.Shared.assetsPath
  val ioRoutes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    
    case req @ GET -> "assets" /: file =>
      StaticFile
        .fromFile(new File(s"$assets/$file"), blocker, Some(req))
        .getOrElseF(NotFound())

    case req @ GET -> Root / "blog" / fileName =>
      StaticFile
        .fromFile(new File(s"${blog.Path.blogs}/$fileName"), blocker, Some(req))
        .getOrElseF(NotFound())
  }
}