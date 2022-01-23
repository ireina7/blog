package blog.core

import blog.*
import blog.core.*
import blog.page
import scalatags.Text.all.{
  title as titleAttr,
  *
}
import scalatags.Text.tags2.title
import cats.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*



trait Generator[F[_]: Monad]
  (using
    fileIO: FileIOString[F],
    parser: Parser[F, page.Index]
  ):
  
  def config: blog.Configuration
  given blog.Configuration = config

  // A test to abstract typeclass constraints and it works!
  // type FileIOParser[F[_], A] = 
  //   (FileIOString[F], Parser[F, Index]) ?=> F[A]

  def readIndex: F[page.Index] = {
    
    for {
      raw <- fileIO.readFile(Path.items)
      res <- parser.parse(raw)
    } yield res
  }

  def generateIndex: F[HtmlText] = {
    
    for {
      index <- readIndex
    } yield div(index.map(page.Frame.item(_)))
  }

  def generateHtml(content: HtmlText = div()): HtmlText = {
    val index = page.Frame.index(content)
    // println(index)
    index
  }

  def generateIndexPage: F[Unit] = {
    for {
      index <-  generateIndex
      _     <-  fileIO.writeFile(
                  s"${Path.staticPackage}/index.html",
                  generateHtml(index).toString
                )
    } yield ()
  }

  def toJson(index: page.Index): F[String] = ???
  def generateIndexFile
    (path: String, index: page.Index): F[Unit] = {
    
    for {
      json <- toJson(index)
      _    <- fileIO.writeFile(path, json)
    } yield ()
  }

end Generator
