package blog.static

import blog.*
import blog.util.*
import blog.page.Frame
import scalatags.Text.all.{
  title as titleAttr,
  *
}
import scalatags.Text.tags2.title
import cats.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*




object Generator {

  given blog.Configuation = blog.Configuation.staticBlog
  
  def generateHtml(content: HtmlText = div()): HtmlText = {
    val index = Frame.index(content)
    // println(index)
    index
  }

  def generateIndexPage[F[_]: Monad]
    (using 
      fileIO: FileIOString[F],
      parser: Parser[F, Index]
    ): F[Unit] = {
      for {
      index <-  generateIndex[F]
      _     <-  fileIO.writeFile(
                  s"${Path.staticPackage}/index.html", 
                  Generator.generateHtml(index).toString
                )
    } yield ()
  }

  // A test to abstract typeclass constraints and it works!
  type FileIOParser[F[_], A] = 
    (FileIOString[F], Parser[F, Index]) ?=> F[A]
  /**
   * Dirty too!
  */
  def writeHtmlToFile[F[_]](path: String)
    (using fileIO: FileIO[F, String, String]): F[Unit] = {
    
    fileIO.writeFile(path, generateHtml(b("hello!!")).toString)
  }

  /**
   * Read in `items.json`
  */
  type Index = List[page.Item]
  def readIndex[F[_]: Monad]
    (using 
      fileIO: FileIOString[F],
      parser: Parser[F, Index]
    ): F[Index] = {
    
    for {
      raw <- fileIO.readFile(Path.items)
      res <- parser.parse(raw)
    } yield res
  }


  def generateIndex[F[_]: Monad]: FileIOParser[F, HtmlText] = {
      
    for {
      index <- readIndex[F]
    } yield div(index.map(page.Frame.item(_)))
  }



  
  /**
   * An example implementation of the effect F[_] inside readIndex
  */
  import cats.effect.IO
  import cats.data.*
  type IOErr[A] = EitherT[IO, Throwable, A]
  given FileIO[IOErr, String, String] with {
    def readFile(path: String) = {
      EitherT.rightT[IO, Throwable](summon[FileIOString[Id]].readFile(path))
    }
    def writeFile(path: String, content: String) = {
      EitherT.rightT[IO, Throwable](summon[FileIOString[Id]].writeFile(path, content))
    }
  }
  given Parser[IOErr, Index] with {
    def parse(s: String): IOErr[Index] = {
      import io.circe.*
      import io.circe.generic.auto.*
      import io.circe.parser.*
      given itemDecoder: Decoder[blog.page.Item] = new Decoder[blog.page.Item]:
        def apply(c: HCursor) =
          for {
            title  <- c.downField("title" ).as[String]
            link   <- c.downField("link"  ).as[String]
            author <- c.downField("author").as[String]
            date   <- c.downField("date"  ).as[String]
            view   <- c.downField("view"  ).as[String]
          } yield {
            blog.page.Item(title, link, author, date, view)
          }
      
      EitherT.fromEither(decode(s))
    }
  }
  // given Functor[IOErr] with {
  //   def map[A, B](fa: IOErr[A])(f: A => B): IOErr[B] = 
  //     fa.map(either => either.map(f(_)))
  // }
  // given Monad[IOErr] with {
  //   def pure[A](a: A): IOErr[A] = IO { Right(a) }
  //   def flatMap[A, B](fa: IOErr[A])(f: A => IOErr[B]): IOErr[B] = {
  //     val either = fa.unsafeRunSync()
  //     either match
  //       case Right(x) => f(x)
  //       case Left(err) => IO { Left(err) }
  //   }
  //   def tailRecM[A, B](a: A)(f: A => IOErr[Either[A, B]]): IOErr[B] =
  //     flatMap(f(a)) {
  //       case Right(b) => pure(b)
  //       case Left(nextA) => tailRecM(nextA)(f)
  //     }
  // }

}