package blog.skeleton

import blog.core.*
import cats.*
import cats.data.*
import cats.effect.IO
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
// import blog.FileIOString
import blog.skeleton.Exprs.SkeleExpr
import blog.skeleton.parser
import blog.skeleton.evaluator



trait SkeletonCompiler [
  F[_]: Monad,
  Output,
  MarkDownEnv,
  ExprEnv,
  GenEnv,
]
(using
  fileIO: FileIOString[F],
  parser: Parser[F, SkeleExpr],
  evalMarkDown: blog.core.Eval[[A] =>> MarkDownEnv ?=> F[A], SkeleExpr, Output],
  evalExpr: blog.core.Eval[[A] =>> ExprEnv ?=> F[A], SkeleExpr, SkeleExpr],
  generator: BlogIndexGenerator[[A] =>> GenEnv ?=> F[A]],
  htmlWriter: blog.core.Writer[F, String, Output],
):
  import fileIO.readFile
  import parser.parse

  def compile(path: String)
    (using ExprEnv, MarkDownEnv): F[Output] = {
    
    for
      text <- readFile(path)
      tree <- parse(text)
      expr <- evalExpr.eval(tree)
      html <- {
        // println(expr.toString)
        evalMarkDown.eval(expr)
      }
    yield html
  }
end SkeletonCompiler



/*

\Math
  \sqrt{1 2}
  \double
  \quoted

\module\Math {
  \set\sqrt {
    \unimplemented
  }
}

This is \Scala\Js

(\list Math) {}

*/



class SkeletonHtml [
  F[_]: Monad,
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
) extends SkeletonCompiler[F, blog.HtmlText, MarkDownEnv, ExprEnv, GenEnv]:
  import blog.page

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
                (s"${generator.config.blogPath}/pages/$fileName")
      _    <- registerIndex(item)
    yield ()

  def registerCmd(path: String, title: String)
    (using ExprEnv, MarkDownEnv, GenEnv): F[Unit] = {
    
    val linkDir = s"${generator.config.blogType.blogPath}"
    val format = java.time.format.DateTimeFormatter.ofPattern("yyyy年 MM月 dd日")
    val item = page.Item(
      title  = title,
      link   = s"$linkDir/$title.html",
      author = "Ireina7",
      date   = java.time.LocalDateTime.now.format(format),
      view   = "",
    )
    register(path, s"$title.html", item)
  }

end SkeletonHtml








object Skeleton:

  def main(args: Array[String]): Unit =

    import Effect.{*, given}
    import Generator.given
    import parser.NaiveParser.given
    import evaluator.PreMarkDownExprEvaluator
    import evaluator.MarkDownEvaluator
    import MarkDownEvaluator.given
    import PreMarkDownExprEvaluator.given

    given blog.Configuration = blog.Configuration.staticBlog
    // given blog.Configuration = blog.Configuration.onlineBlog
    given markdownEnv: MarkDownEvaluator.Environment =
      MarkDownEvaluator.Environment.predef
    given exprEnv: PreMarkDownExprEvaluator.Environment = 
      PreMarkDownExprEvaluator.Environment.predefForMarkDown

    val console = summon[Console[IOErr]]
    def log(s: String) = console.log(s).run()

    if(args.length < 3) {
      log("[error] Too few CLI argument. No command executed.")
      return ()
    }
    // println(args.toList)
    val filePath = args(1)
    val fileTitle = args(2)

    // given generator = summon[Generator[GenEffect]]
    val skeleton: SkeletonHtml[
      IOErr,
      // blog.HtmlText,
      MarkDownEvaluator.Environment,
      PreMarkDownExprEvaluator.Environment,
      blog.Configuration,
    ] = 
      new SkeletonHtml
    
//    val link = summon[blog.Configuration].blogType.blogPath
    // println(link)
    val exe = 
      skeleton.registerCmd(
        filePath, 
        fileTitle,
      )
    // println("end exe")
    exe.run() match
      case Left(err) => println(s"[error] $err")
      case Right(_)  => println(s"[info] Ok: $filePath registered.")

  end main
    
end Skeleton






