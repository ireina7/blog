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


// def pack(): Unit = {
//   Generator.writeHtmlToFile("./shared/public/index.html")
// }

object Generator:

  given blog.Configuation = blog.Configuation.staticBlog
  
  def generateHtml(content: HtmlText = div()): HtmlText = {
    val index = Frame.index(content)
    println(index)
    index
  }

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
  def readIndex[F[_]: Monad]()
    (using 
      fileIO: FileIOString[F],
      jsonParser: From[F, String, Index]
    ): F[Index] = {
    
    for {
      raw <- fileIO.readFile(Path.items)
      res <- jsonParser.from(raw)
    } yield res
  }


  
  /**
   * An example implementation of the effect F[_] inside readIndex
  */
  import cats.effect.IO
  type IOErr[A] = IO[Either[Throwable, A]]
  given FileIO[IOErr, String, String] with {
    def readFile(path: String) = {
      summon[FileIO[IO, String, String]].readFile(path).map(Right(_))
    }
    def writeFile(path: String, content: String) = {
      summon[FileIO[IO, String, String]].writeFile(path, content).map(Right(_))
    }
  }
  given From[IOErr, String, Index] with {
    def from(s: String): IOErr[Index] = {
      import io.circe.*
      import io.circe.generic.auto.*
      import io.circe.parser.*
      given itemDecoder: Decoder[blog.page.Item] = new Decoder[blog.page.Item]:
        def apply(c: HCursor) =
          for {
            title  <- c.downField("title" ).as[String]
            author <- c.downField("author").as[String]
            date   <- c.downField("date"  ).as[String]
            view   <- c.downField("view"  ).as[String]
          } yield {
            blog.page.Item(title, author, date, view)
          }
      
      IO { decode(s) }
    }
  }
  given Functor[IOErr] with {
    def map[A, B](fa: IOErr[A])(f: A => B): IOErr[B] = 
      fa.map(either => either.map(f(_)))
  }
  given Monad[IOErr] with {
    def pure[A](a: A): IOErr[A] = IO { Right(a) }
    def flatMap[A, B](fa: IOErr[A])(f: A => IOErr[B]): IOErr[B] = {
      val either = fa.unsafeRunSync()
      either match
        case Right(x) => f(x)
        case Left(err) => IO { Left(err) }
    }
    def tailRecM[A, B](a: A)(f: A => IOErr[Either[A, B]]): IOErr[B] =
      flatMap(f(a)) {
        case Right(b) => pure(b)
        case Left(nextA) => tailRecM(nextA)(f)
      }
  }


end Generator
