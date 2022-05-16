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
// import http4s.dsl.*
// import http4s.dsl.io.*
import http4s.server
import http4s.syntax.kleisli.*
import server.blaze.BlazeServerBuilder
import server.Server
import server.staticcontent.*
import scala.concurrent.ExecutionContext
import cats.Monad
import cats.implicits.*




object Routes:

  import java.util.concurrent.*
  import blog.server.data.*
  given conf: blog.Configuration = blog.Configuration.onlineBlog
  val blockingPool = Executors.newFixedThreadPool(4)
  val blocker = Blocker.liftExecutorService(blockingPool)

  def mainRoutes
    [F[_]: Sync: ContextShift: Monad]
    : HttpRoutes[F] =
    
    val dsl = new Http4sDsl[F]{}
    import dsl.*
    import blog.page

    HttpRoutes.of[F] {
      // /
      case req @ GET -> Root =>
        StaticFile
          .fromFile(new File(s"${blog.Path.blogs}/pages/index.html"), blocker, Some(req))
          .getOrElseF(NotFound())

      // /assets/$file
      case req @ GET -> "assets" /: file =>
        val assets = blog.Path.assets
        StaticFile
          .fromFile(new File(s"$assets/$file"), blocker, Some(req))
          .getOrElseF(NotFound())

      case GET -> Root / "conf" =>
        Ok(
          page.Frame.index(
            blog.Configuration.onlineBlog.toString
          )
        )
      
      case GET -> Root / "login" => 
        Ok(page.Login.index)

      case req @ POST -> Root / "login" / "submit" => {
        import org.http4s.FormDataDecoder.*
        given fooMapper: FormDataDecoder[LoginMessage] = (
          field[String]("userName"),
          field[String]("password"),
        ).mapN(LoginMessage.apply)
        for
          msg <- req.as[LoginMessage]
          res <- if(msg.userName == "ireina" && msg.password == "elpsycongroo") 
            then Ok(page.ControlPanel.index) 
            else Ok(page.Frame.index(div("Permission denied...")))
        yield res
      }


      // /about
      case GET -> Root / "about" =>
        Ok(page.About.index)
        
      // /filter
      case GET -> Root / "filter" =>
        Ok(page.Filter.index(
          span(color := "grey")("No result")
        ))
      
      // /skeleton
      case GET -> Root / "skeleton" => 
        val content = page.SkeletonRepl.index
        Ok(content)
          
      // /blog/$file
      case req @ GET -> "blog" /: fileName =>
        StaticFile
          .fromFile(new File(s"${blog.Path.blogs}/pages/$fileName"), blocker, Some(req))
          .getOrElseF(NotFound())
            
      // /compile
      case req @ POST -> Root / "compile" => for {
        code <- req.as[String]
        res  <- Ok(page.Frame.index(code))
      } yield res
      
      // /shutdown
      case GET -> Root / "shutdown" =>
        sys.exit()
      
      // case GET -> Root / "view" =>
      //   val content = Frame.index(
      //     div(
      //       for(i <- 0 to 10) yield
      //         Frame.item(blog.page.Item(
      //           title = s"第${i}篇文章！", 
      //           link = "???",
      //           author = "Ireina", 
      //           date = java.util.Date().toString,
      //           view = "")
      //         ),
      //     )
      //   )
      //   Ok(content)
    }
  end mainRoutes

  
  // import java.util.concurrent.*
  // val blockingPool = Executors.newFixedThreadPool(4)
  // val blocker = Blocker.liftExecutorService(blockingPool)
  // implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  // val dsl = new Http4sDsl[IO]{}
  // import dsl.*
  // val assets = blog.Shared.assetsPath
  // val ioRoutes: HttpRoutes[IO] = HttpRoutes.of[IO] {

  //   case req @ GET -> Root / "blog" / fileName =>
  //     StaticFile
  //       .fromFile(new File(s"${blog.Path.blogs}/$fileName"), blocker, Some(req))
  //       .getOrElseF(NotFound())
  // }

end Routes