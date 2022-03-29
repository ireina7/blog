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
  
  given given_defaultGenerator[F[_]: Monad, Path, Thing](using
      fileIO: FileIO[F, Path, String],
      parser: Parser[F, Thing],
      decoder: Decoder[F, Thing],
    )
    : Generator_[F, Path, Thing] = new Generator_ {
    
    override def read(path: Path): F[Thing] = {
      for {
        raw <- fileIO.readFile(path)
        res <- parser.parse(raw)
      } yield res
    }

    override def write(x: Thing)(path: Path) = {
      for {
        json <- decoder.decode(x)
        _    <- fileIO.writeFile(path, json)
      } yield ()
    }
  }

end Generator_



trait BlogIndexGenerator[F[_]: Monad]
  (using
    gen: Generator_[F, String, page.Index],
    htmlWriter: Writer[F, String, blog.HtmlText],
  ):
  
  def config: blog.Configuration
  given blog.Configuration = config

  // A test to abstract typeclass constraints and it works!
  // type FileIOParser[F[_], A] = 
  //   (FileIOString[F], Parser[F, Index]) ?=> F[A]

  def readIndex: F[page.Index] = 
    gen.read(Path.items)

  def readIndexHtml: F[HtmlText] = {
    
    for {
      index <- readIndex
    } yield div(index.map(page.Frame.item(_)))
  }

  def indexPage(content: HtmlText = div()): HtmlText = 
    page.Frame.index(content)

  
  def generateIndexPage: F[Unit] = {
    // println(s"${config.blogPath}/index.html")
    for {
      index <-  readIndexHtml
      _     <-  htmlWriter.write
                  (page.Frame.index(index))
                  (s"${config.blogPath}/index.html")
    } yield ()
  }
  
  def generateIndexFile
    (path: String, index: page.Index): F[Unit] = {
    
    gen.write(index)(path)
  }

end BlogIndexGenerator




object Generator:

  import Effect.*
  import Effect.given
  given (using conf: blog.Configuration): BlogIndexGenerator[IOErr] with
    def config = conf


  given (using conf: blog.Configuration):
    BlogIndexGenerator[[A] =>> Injection[IOErr, blog.Configuration, A]] with
    def config = conf

end Generator

