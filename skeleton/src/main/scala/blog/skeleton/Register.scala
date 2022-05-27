package blog.skeleton

import blog.core
import blog.core.Effect.*
import blog.core.Effect.given
import blog.core.*
import cats.*
import cats.data.*
import cats.effect.IO
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import blog.skeleton.Exprs.SkeleExpr
import blog.skeleton.parser
import blog.skeleton.evaluator
import blog.skeleton.compiler.Compiler
import blog.skeleton.compiler.MarkDownCompiler



final class HtmlRegister[F[_], MarkDownEnv, ExprEnv, GenEnv]
  (using
    conf: blog.Configuration,
    M: MonadError[F, Throwable],
    fileIO: FileIOString[F],
    compiler: MarkDownCompiler[F, MarkDownEnv, ExprEnv, blog.HtmlText],
    generator: BlogIndexGenerator[[A] =>> GenEnv ?=> F[A]],
    htmlWriter: core.Writer[F, String, blog.HtmlText],
  ):
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

  def registerNewId(using GenEnv): F[Int] = 
    for
      exists <- fileIO.existFile(conf.path.index)
      idx    <- 
        (if !exists then generator.createIndex else ().pure) >> generator.readIndex
      idNum  <- generator.readIndex.map(xs => xs.map(_.id).maxOption.getOrElse(-1) + 1)
    yield idNum

  def registerDirectory
    (path: String, title: String, blogNo: Option[Int] = None)
    (using ExprEnv, MarkDownEnv, GenEnv): F[Unit] = 
    import blog.blogPath
    import blog.blogRoute
    import fileIO.*

    val linkDir = s"${generator.config.blogRoute}"
    val storePath = s"${generator.config.blogPath}/pages"
    // println(storePath)
    val format = java.time.format.DateTimeFormatter.ofPattern("yyyy年 MM月 dd日")
    for
      ok   <- existDirectory(s"${generator.config.blogPath}")
      _    <- if ok then ().pure 
              else  M.raiseError(
                      blog.Error(s"blog path ${generator.config.blogPath} does not exist.")
                    )
      html <- compiler.compileFile(s"$path/index.skele")
      id   <- blogNo.map(_.pure).getOrElse(registerNewId)
      _    <- createDirectory(storePath)
      _    <- createDirectory(s"$storePath/$id")
      _    <- copyDirectory(path, s"$storePath/$id")
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

  def registerFile
    (path: String, title: String, blogNo: Option[Int] = None)
    (using ExprEnv, MarkDownEnv, GenEnv): F[Unit] = 
    // println(generator.config.blogPath)
    import blog.blogPath
    import blog.blogRoute

    val linkDir = s"${generator.config.blogRoute}"
    val format = java.time.format.DateTimeFormatter.ofPattern("yyyy年 MM月 dd日")
    for
      html <- compiler.compileFile(path)
      id   <- blogNo.map(_.pure).getOrElse(registerNewId)
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

  def registerString
    (src: String, title: String, blogNo: Option[Int] = None)
    (using ExprEnv, MarkDownEnv, GenEnv): F[Unit] = 
    import blog.blogPath
    import blog.blogRoute
    import fileIO.*

    val linkDir = s"${generator.config.blogRoute}"
    val storePath = s"${generator.config.blogPath}/pages"
    // println(storePath)
    val format = java.time.format.DateTimeFormatter.ofPattern("yyyy年 MM月 dd日")
    for
      ok   <- existDirectory(s"${generator.config.blogPath}")
      _    <- if ok then ().pure 
              else  M.raiseError(
                      blog.Error(s"blog path ${generator.config.blogPath} does not exist.")
                    )
      html <- compiler.compile(src)
      id   <- { println(html); blogNo.map(_.pure).getOrElse(registerNewId) }
      _    <- createDirectory(storePath)
      _    <- createDirectory(s"$storePath/$id")
      // _    <- copyDirectory(path, s"$storePath/$id")
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
  end registerString

  def register
    (path: String, title: String, blogNo: Option[Int] = None)
    (using ExprEnv, MarkDownEnv, GenEnv): F[Unit] = 
    registerDirectory(path, title, blogNo)

end HtmlRegister


object HtmlRegister:
  import Effect.{*, given}
  import Generator.given
  import parser.NaiveParser.given
  import evaluator.PreMarkDownExprEvaluator
  import evaluator.MarkDownEvaluator
  import MarkDownEvaluator.given
  import PreMarkDownExprEvaluator.given

  given blog.Configuration = blog.Configuration.onlineBlog

  val htmlRegisterIOErr: HtmlRegister[
    IOErr,
    MarkDownEvaluator.Environment,
    PreMarkDownExprEvaluator.Environment,
    blog.Configuration,
  ] = new HtmlRegister

end HtmlRegister

