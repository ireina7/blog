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



trait Generator[F[_]: Monad, Path, Thing] extends
  Reader[F, Path, Thing],
  Writer[F, Path, Thing]



object Generator:

  class DefaultGenerator[F[_]: Monad, Path, Thing]
    (using 
      fileIO: FileIO[F, Path, String],
      decoder: Decoder[F, Thing],
      encoder: Encoder[F, Thing],
    )
    extends Generator[F, Path, Thing]:

    override def read(path: Path): F[Thing] = 
      for {
        raw <- fileIO.readFile(path)
        res <- decoder.decode(raw)
      } yield res

    override def write(x: Thing)(path: Path) = 
      for {
        json <- encoder.encode(x) // json for example
        _    <- fileIO.writeFile(path, json)
      } yield ()
  end DefaultGenerator
  

  given given_defaultGenerator
    [F[_]: Monad, Path, Thing](using
      fileIO: FileIO[F, Path, String],
      decoder: Decoder[F, Thing],
      encoder: Encoder[F, Thing],
    )
    : DefaultGenerator[F, Path, Thing] = 
    new DefaultGenerator

end Generator



class BlogIndexGenerator[F[_]: Monad] 
  (using
    // gen: Generator[F, String, page.Index],
    fileIO: FileIO[F, String, String],
    decoder: Decoder[F, page.Index],
    encoder: Encoder[F, page.Index],
    htmlWriter: Writer[F, String, blog.HtmlText],
    val config: blog.Configuration
  ) extends Generator[F, String, page.Index]:
  
  // def config: blog.Configuration
  // given blog.Configuration = config

  // A test to abstract typeclass constraints and it works!
  // type FileIOParser[F[_], A] = 
  //   (FileIOString[F], Parser[F, Index]) ?=> F[A]

  override def read(path: String): F[page.Index] = 
    for {
      raw <- fileIO.readFile(path)
      res <- decoder.decode(raw)
    } yield res

  override def write(x: page.Index)(path: String) = 
    for {
      json <- encoder.encode(x) // json for example
      _    <- fileIO.writeFile(path, json)
    } yield ()

  
  // override def read(path: String): F[page.Index] =
  //   read(path) 

  // override def write(x: page.Index)(path: String) =
  //   write(x)(path) 

  def readIndex: F[page.Index] = 
    read(config.path.index)

  def createIndex: F[Unit] =
    fileIO.writeFile(config.path.index, "[]")

  def readIndexHtml: F[HtmlText] = {
    
    for {
      index <- readIndex
    } yield div(
      index.map(blog => 
        div(page.Frame.item(blog), hr)
      )
    )
  }

  def indexPage(content: HtmlText = div()): HtmlText = 
    page.Frame.index(content)

  
  def generateIndexPage: F[Unit] = {
    // println(s"${config.blogPath}/index.html")
    for {
      index <-  readIndexHtml
      _     <-  htmlWriter.write
                  (page.Frame.index(index))
                  (s"${config.blogPath}/pages/index.html")
    } yield ()
  }
  
  def generateIndexFile
    (path: String, index: page.Index): F[Unit] = {
    
    write(index)(path)
  }

end BlogIndexGenerator



object BlogIndexGenerator:

  import Effect.*
  import Effect.given
  given [F[_]: Monad](using 
    conf: blog.Configuration,
    fileIO: FileIO[F, String, String],
    decoder: Decoder[F, page.Index],
    encoder: Encoder[F, page.Index],
    htmlWriter: Writer[F, String, blog.HtmlText],
  ): BlogIndexGenerator[F] =
    new BlogIndexGenerator


  // given (using conf: blog.Configuration):
  //   BlogIndexGenerator[[A] =>> Injection[IOErr, blog.Configuration, A]] =
  //     new BlogIndexGenerator

end BlogIndexGenerator



