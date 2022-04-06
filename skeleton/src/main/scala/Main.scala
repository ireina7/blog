package blog.skeleton

import cats.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.effect.IO
import cats.data.*

import blog.core.*
// import blog.FileIOString
import blog.skeleton.Exprs.SkeleExpr



trait Skeleton
  [
    F[_]: Monad, 
    // Output,
    MarkDownEnv,
    ExprEnv,
    GenEnv,
  ]
  (using
    fileIO: FileIOString[F],
    parser: Parser[F, SkeleExpr],
    evalMarkDown: blog.core.Eval[[A] =>> MarkDownEnv ?=> F[A], SkeleExpr, blog.HtmlText],
    evalExpr: blog.core.Eval[[A] =>> ExprEnv ?=> F[A], SkeleExpr, SkeleExpr],
    generator: BlogIndexGenerator[[A] =>> GenEnv ?=> F[A]],
    htmlWriter: blog.core.Writer[F, String, blog.HtmlText],
  ):
  import fileIO.{ readFile, writeFile }
  import parser.parse
  import blog.page


  def compile(path: String)
    (using ExprEnv, MarkDownEnv): F[blog.HtmlText] = {
    
    for {
      text <- readFile(path)
      tree <- parse(text)
      expr <- evalExpr.eval(tree)
      html <- { 
        // println(expr)
        evalMarkDown.eval(expr)
      }
    } yield html
  }

  def registerIndex(item: page.Item)
    (using GenEnv): F[Unit] = {
    
    for
      idx  <- generator.readIndex
      _    <- generator.generateIndexFile(
                blog.Path.items,
                item :: idx
              )
      _    <- generator.generateIndexPage
    yield ()
  }

  def register(path: String, fileName: String, item: page.Item)
    (using ExprEnv, MarkDownEnv, GenEnv): F[Unit] = 
    // println(generator.config.blogPath)
    for
      html <- compile(path)
      _    <- htmlWriter.write
                (generator.indexPage(html))
                (s"${generator.config.blogPath}/$fileName")
      _    <- registerIndex(item)
    yield ()

  def registerCmd(path: String, title: String)
    (using ExprEnv, MarkDownEnv, GenEnv): F[Unit] = {
    
    val linkDir = generator.config.blogType.blogPath
    val format = java.time.format.DateTimeFormatter.ofPattern("yyyy年 MM月 dd日")
    val item = page.Item(
      title  = title,
      link   = s"$linkDir/$title.html",
      author = "Ireina7",
      date   = java.time.LocalDateTime.now.format(format).toString,
      view   = "",
    )
    register(path, s"$title.html", item)
  }

end Skeleton








object Skeleton:

  def main(args: Array[String]): Unit =

    import Effect.{*, given}
    import MarkDownEvaluator.given
    import PreMarkDownExprEvaluator.given
    import Generator.given

    given blog.Configuration = blog.Configuration.staticBlog
    given markdownEnv: MarkDownEvaluator.Environment =
      MarkDownEvaluator.Environment.predef
    given exprEnv: PreMarkDownExprEvaluator.Environment = 
      PreMarkDownExprEvaluator.Environment.predefForMarkDown

    val console = summon[Console[IOErr]]
    def log(s: String) = console.log(s).run()

    if(args.length < 3) {
      log("No command executed.")
      return ()
    }
    println(args.toList)
    val filePath = args(1)
    val fileTitle = args(2)

    // given generator = summon[Generator[GenEffect]]
    val skeleton: Skeleton[
      IOErr,
      // blog.HtmlText,
      MarkDownEvaluator.Environment,
      PreMarkDownExprEvaluator.Environment,
      blog.Configuration,
    ] = 
      new Skeleton {}
    
    val link = summon[blog.Configuration].blogType.blogPath
    // println(link)
    val exe = 
      skeleton.registerCmd(
        filePath, 
        fileTitle,
      )
    // println("end exe")
    exe.run() match
      case Left(err) => println(s"$err")
      case Right(ok) => println(s"Ok: $ok")

  end main
    
end Skeleton

