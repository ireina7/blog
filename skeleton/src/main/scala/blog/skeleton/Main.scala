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




final class SkeletonHtml[F[_]: Monad, MarkDownEnv, ExprEnv, GenEnv]
(using
  conf: blog.Configuration,
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
      exists <- fileIO.existFile(conf.path.index)
      idx    <- 
        (if !exists then generator.createIndex else ().pure) >> generator.readIndex
      _      <- generator.generateIndexFile(conf.path.index, item :: idx.filter(_.id != item.id))
      _      <- generator.generateIndexPage
    yield ()
  }

  def registerFile
    (path: String, title: String, blogNo: Option[Int] = None)
    (using ExprEnv, MarkDownEnv, GenEnv): F[Unit] = 
    // println(generator.config.blogPath)
    import blog.blogPath
    import blog.blogRoute

    val linkDir = s"${generator.config.blogRoute}"
    val format = java.time.format.DateTimeFormatter.ofPattern("yyyy年 MM月 dd日")
    for
      html <- compile(path)
      id   <- blogNo match
                case Some(i) => i.pure
                case None => 
                  for
                    exists <- fileIO.existFile(conf.path.index)
                    idx    <- 
                      (if !exists then generator.createIndex else ().pure) >> generator.readIndex
                    idNum  <- generator.readIndex.map(xs => xs.map(_.id).maxOption.getOrElse(-1) + 1)
                  yield idNum

      _    <- fileIO.createDirectory(s"${generator.config.blogPath}/pages")
      _    <- htmlWriter.write
                (generator.indexPage(html))
                (s"${generator.config.blogPath}/pages/blog-$id.html")
      _    <- registerIndex(page.Item(
                id     = id, 
                title  = title,
                link   = s"$linkDir/blog-$id.html",
                author = "Ireina7",
                date   = java.time.LocalDateTime.now.format(format),
                view   = "",
              ))
    yield ()
  end registerFile

  def registerDirectory
    (path: String, title: String, blogNo: Option[Int] = None)
    (using ExprEnv, MarkDownEnv, GenEnv): F[Unit] = 
    // println(generator.config.blogPath)
    import blog.blogPath
    import blog.blogRoute

    val linkDir = s"${generator.config.blogRoute}"
    val storePath = s"${generator.config.blogPath}/pages"
    val format = java.time.format.DateTimeFormatter.ofPattern("yyyy年 MM月 dd日")
    for
      html <- compile(s"$path/index.skele")
      id   <- blogNo match
                case Some(i) => i.pure
                case None => 
                  for
                    exists <- fileIO.existFile(conf.path.index)
                    idx    <- 
                      (if !exists then generator.createIndex else ().pure) >> generator.readIndex
                    idNum  <- generator.readIndex.map(xs => xs.map(_.id).maxOption.getOrElse(-1) + 1)
                  yield idNum
      _    <- fileIO.createDirectory(storePath)
      _    <- fileIO.createDirectory(s"$storePath/$id")
      _    <- fileIO.copyDirectory(path, s"$storePath/$id")
      _    <- htmlWriter.write
                (generator.indexPage(html))
                (s"$storePath/$id/index.html")
      _    <- registerIndex(page.Item(
                id     = id, 
                title  = title,
                link   = s"$linkDir/$id/index.html",
                author = "Ireina7",
                date   = java.time.LocalDateTime.now.format(format),
                view   = "",
              ))
    yield ()
  end registerDirectory

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

    // given blog.Configuration = blog.Configuration.staticBlog
    given blog.Configuration = blog.Configuration.onlineBlog
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
    val blogNo: Option[Int] = 
      if args.length > 3 
      then Some(args(3).toInt) 
      else None

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
    // val exe = 
    //   skeleton.registerFile(filePath, fileTitle, blogNo)

    val exe = 
      skeleton.registerDirectory(filePath, fileTitle, blogNo)
    
    exe.run() match
      case Left(err) => println(s"[error] $err")
      case Right(_)  => println(s"[info] Ok: $filePath registered.")

  end main
    
end Skeleton






