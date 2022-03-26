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



trait Generator_[F[_]: Monad, Path, Thing] extends
  Reader[F, Path, Thing],
  Writer[F, Path, Thing]



object Generator_ :
  
  given given_indexGenerator[F[_]: Monad, Path](using
      fileIO: FileIO[F, Path, String],
      parser: Parser[F, page.Index],
      decoder: Decoder[F, page.Index],
    )
    : Generator_[F, Path, page.Index] = new Generator_ {
    
    override def read(path: Path): F[page.Index] = {
      for {
        raw <- fileIO.readFile(path)
        res <- parser.parse(raw)
      } yield res
    }

    override def write(x: page.Index)(path: Path) = {
      for {
        json <- decoder.decode(x)
        _    <- fileIO.writeFile(path, json)
      } yield ()
    }
  }

end Generator_



trait Generator[F[_]: Monad]
  (using
    fileIO: FileIOString[F],
    parser: Parser[F, page.Index],
    decoder: Decoder[F, page.Index],
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
    // println(s"${config.blogPath}/index.html")
    for {
      index <-  generateIndex
      _     <-  fileIO.writeFile(
                  s"${config.blogPath}/index.html",
                  generateHtml(index).toString
                )
    } yield ()
  }

  
  def generateIndexFile
    (path: String, index: page.Index): F[Unit] = {
    
    for {
      json <- decoder.decode(index)
      _    <- fileIO.writeFile(path, json)
    } yield ()
  }

end Generator




object Generator:

  import Effect.*
  import Effect.given
  given (using conf: blog.Configuration): Generator[IOErr] with
    def config = conf


  given (using conf: blog.Configuration):
    Generator[[A] =>> Injection[IOErr, blog.Configuration, A]] with
    def config = conf

end Generator

