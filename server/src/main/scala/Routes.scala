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
import blog.skeleton.compiler.MarkDownCompiler
import blog.core.*
import blog.skeleton.Exprs.SkeleExpr




object Routes:

  import java.util.concurrent.*
  import blog.server.data.*
  import blog.core.Effect.{*, given}
  import blog.skeleton.evaluator.PreMarkDownExprEvaluator
  import blog.skeleton.evaluator.MarkDownEvaluator
  given conf: blog.Configuration = blog.Configuration.onlineBlog
  given markdownEnv: MarkDownEvaluator.Environment = 
    MarkDownEvaluator.Environment.predef
  var exprEnvEvil: PreMarkDownExprEvaluator.Environment = 
    PreMarkDownExprEvaluator.Environment.predefForMarkDown
  given exprEnv: PreMarkDownExprEvaluator.Environment = 
    exprEnvEvil
  val blockingPool = Executors.newFixedThreadPool(4)
  val blocker = Blocker.liftExecutorService(blockingPool)

  var hasLogin: Boolean = false

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
        if hasLogin
        then Ok(page.ControlPanel.index)
        else Ok(page.Login.index)

      case GET -> Root / "logout" => 
        Ok(
          page.Frame.index(
            div("Logged out.")
          )
        )

      case req @ POST -> Root / "login" / "submit" => {
        import org.http4s.FormDataDecoder.*
        given fooMapper: FormDataDecoder[LoginMessage] = (
          field[String]("userName"),
          field[String]("password"),
        ).mapN(LoginMessage.apply)
        for
          msg <- req.as[LoginMessage]
          res <- if(msg.userName == "ireina" && msg.password == "elpsycongroo") 
            then 
              hasLogin = true
              Ok(page.ControlPanel.index) 
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
        // Refresh environment
        exprEnvEvil = 
          PreMarkDownExprEvaluator.Environment.predefForMarkDown
        val content = page.SkeletonRepl.index
        Ok(content)
          
      // /blog/$file
      case req @ GET -> "blog" /: fileName =>
        StaticFile
          .fromFile(new File(s"${blog.Path.blogs}/pages/$fileName"), blocker, Some(req))
          .getOrElseF(NotFound())
            

      // case req @ POST -> Root / "compile" => {
      //   println()
      //   Ok("")
      // }
      // /compile
      // case req @ POST -> Root / "compile" => {
      //   import org.http4s.FormDataDecoder.*
      //   import blog.core.Effect.{*, given}
      //   import blog.skeleton.evaluator.PreMarkDownExprEvaluator
      //   import blog.skeleton.evaluator.MarkDownEvaluator

      //   given fooMapper: FormDataDecoder[String] = field[String]("src")
      //   given markdownEnv: MarkDownEvaluator.Environment = 
      //     MarkDownEvaluator.Environment.predef
      //   given exprEnv: PreMarkDownExprEvaluator.Environment = 
      //     PreMarkDownExprEvaluator.Environment.predefForMarkDown

      //   val compiler: blog.skeleton.MarkdownCompiler[
      //     F,
      //     blog.HtmlText, 
      //     MarkDownEvaluator.Environment,
      //     PreMarkDownExprEvaluator.Environment
      //   ] = new MarkdownCompiler

      //   for
      //     code <- req.as[String]
      //     res  <- Ok(page.Frame.index(code))
      //   yield res
      // }
      
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

  
  import java.util.concurrent.*
  // val blockingPool = Executors.newFixedThreadPool(4)
  // val blocker = Blocker.liftExecutorService(blockingPool)
  implicit val cs: ContextShift[IO] = 
    IO.contextShift(ExecutionContext.global)

  val dsl = new Http4sDsl[IO]{}
  import dsl.*
  val assets = blog.Shared.assetsPath
  val ioRoutes: HttpRoutes[IO] = HttpRoutes.of[IO] {

    // /compile
    // case req @ POST -> Root / "compile" => {
    //   import org.http4s.FormDataDecoder.*
    //   import blog.core.Effect.{*, given}
    //   import blog.skeleton.evaluator.PreMarkDownExprEvaluator
    //   import blog.skeleton.evaluator.MarkDownEvaluator

    //   given codeDecoder: FormDataDecoder[String] = field[String]("src")
    //   given markdownEnv: MarkDownEvaluator.Environment = 
    //     MarkDownEvaluator.Environment.predef
    //   given exprEnv: PreMarkDownExprEvaluator.Environment = 
    //     PreMarkDownExprEvaluator.Environment.predefForMarkDown
      
    //   // println(s">>>>> ${req.bodyText.compile.toVector.unsafeRunSync}")
    //   val compiler = 
    //     blog.skeleton.MarkdownCompiler.given_HtmlCompilerIOErr
    //   for
    //     code <- req.as[String]
    //     html <- compiler.compile(code).value
    //     res  <- Ok(blog.page.Frame.index(html match {
    //       case Left(err) => err.toString
    //       case Right(ss) => ss
    //     }))
    //   yield res
    // }

    case req @ POST -> Root / "submit" => {
      import org.http4s.FormDataDecoder.*
      import blog.skeleton.Exprs.SkeleExpr.*
      
      given codeDecoder: FormDataDecoder[(String, String, String)] = 
        (field[String]("src"), field[String]("name"), field[String]("title"))
          .mapN((a, b, c) => (a, b, c))
      
      val register = 
        blog.skeleton.HtmlRegister.htmlRegisterIOErr

      for
        (code, name, title) <- req.as[(String, String, String)]
        res <-  
          if true
          then {
            val boxedCode = s"\\box{$code}"
            register.registerString(boxedCode, title)(using exprEnvEvil)
              .value 
              .flatMap {
                case Right(_)  => 
                  Ok(span(style := "color:green;font-family:monospace;font-size:18;")("Ok registered.").toString)
                case Left(err) => 
                  Ok(span(style := "color:red;font-family:monospace;font-size:18;")(err.toString))
              }
          }
          else Ok(span(style := "color:red;font-family:monospace;font-size:18;")("Permission denied. You have to login first."))
      yield res
    }

    case req @ POST -> Root / "compile" => {
      import org.http4s.FormDataDecoder.*
      import blog.skeleton.Exprs.SkeleExpr.*
      
      given codeDecoder: FormDataDecoder[(String, String)] = 
        (field[String]("src"), field[String]("name"))
          .mapN((a, b) => (a, b))

      // println(s">>>>> ${req.bodyText.compile.toVector.unsafeRunSync}")
      val compiler = 
        MarkDownCompiler.htmlCompilerIOErr
      for
        (code, name) <- req.as[(String, String)]
        // expr <- PreMarkDownExprEvaluator.eval
        html <- {
          val evalExpr = compiler
            .eval(s"\\box {$code}")
              (using summon[blog.Configuration])
              (using exprEnvEvil)
          // if name == ""
          // then evalExpr.value
          // else (
          (
            compiler.eval(s"(\\set $name $code)")
              (using summon[blog.Configuration])
              (using exprEnvEvil) >> 
            compiler.eval(s"${name.takeWhile(_ != '{')}")
              (using summon[blog.Configuration])
              (using exprEnvEvil).flatMap { e =>
              div(
                span(style := "color:green;font-family:monospace;font-size:18;")(
                  s"\\set$name:"
                ),
                div(e),
              ).pure
            }
          ).value
        }
        res  <- Ok(html match {
          case Right(ss) => div(ss).toString
          case Left(err) => span(style := "color:red;font-family:monospace;font-size:18;")(
            err.toString
          ).toString
        })
      yield res
    }

    // case req @ GET -> Root / "blog" / fileName =>
    //   StaticFile
    //     .fromFile(new File(s"${blog.Path.blogs}/$fileName"), blocker, Some(req))
    //     .getOrElseF(NotFound())
  }

end Routes