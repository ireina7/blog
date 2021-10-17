package blog.static

import blog.*
import blog.page.Frame
import scalatags.Text.all.{
  title => titleAttr,
  _
}
import scalatags.Text.tags2.title
import cats.Id
import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.functor.*


// def pack(): Unit = {
//   Generator.writeHtmlToFile("./shared/public/index.html")
// }

object Generator {

  given blog.Configuation = blog.Configuation.staticBlog
  
  def generateHtml(content: HtmlText = div()): HtmlText = {
    val index = Frame.index(content)
    index
  }

  /**
   * Dirty too!
  */
  def writeHtmlToFile[F[_]](path: String)(using fileIO: FileIO[F, String, String]): Unit = {
    fileIO.writeFile(path, generateHtml().toString)
  }

  /**
   * Read in `items.json`
  */
  def readIndex[F[_]: Monad]()(using fileIO: FileIO[F, String, String]): blog.Result[HtmlText] = {
    import io.circe.*
    import io.circe.generic.auto.*
    import io.circe.parser.*
    // import io.circe.generic.semiauto.*

    given itemDecoder: Decoder[page.Item] = new Decoder[page.Item]:
      def apply(c: HCursor) =
        for {
          title  <- c.downField("title" ).as[String]
          author <- c.downField("author").as[String]
          date   <- c.downField("date"  ).as[String]
          view   <- c.downField("view"  ).as[String]
        } yield {
          page.Item(title, author, date, view)
        }
    
    val parseResult = for {
      raw <- fileIO.readFile(Path.items)
    } yield {
      decode[List[page.Item]](raw)
    }
    // val raw = ev.readFile(Path.items)
    // val parseResult = decode[List[page.Item]](raw)
    println(parseResult)
    Right(div())
  }

}
