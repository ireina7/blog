package blog.static

import blog.*
import blog.util.*
import blog.page.Frame
import scalatags.Text.all.{
  title => titleAttr,
  _
}
import scalatags.Text.tags2.title
import cats.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*


// def pack(): Unit = {
//   Generator.writeHtmlToFile("./shared/public/index.html")
// }

object Generator {

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
  def readIndex[F[_]: Monad]()
    (using fileIO: FileIO[F, String, String])
    (using jsonParser: From[F, String, List[page.Item]])
    : F[List[page.Item]] = {
    
    for {
      raw <- fileIO.readFile(Path.items)
      res <- jsonParser.from(raw)
    } yield {
      res
    }
    // Right(div())
  }

}
