package blog.static

import blog.*
import blog.page.Frame
import scalatags.Text.all.{
  title => titleAttr,
  _
}
import scalatags.Text.tags2.title



def pack(): Unit = {
  Generator.writeHtmlToFile("./shared/public/index.html")
}

object Generator {

  given blog.Configuation = blog.Configuation.staticBlog
  
  def generateHtml(content: HtmlText = div()): HtmlText = {
    val index = Frame.index(content)
    index
  }

  /**
   * This is dirty!
  */
  def writeToFile(path: String, content: String): Unit = {
    import java.io.PrintWriter
    Some(new PrintWriter(path)).foreach { p => 
      p.write(content)
      p.close
    }
  }

  /**
   * This is also dirty
  */
  def readFile(path: String): String = {
    import java.nio.file.{ Files, Paths }

    new String(Files.readAllBytes(Paths.get(path)))
  }

  /**
   * Dirty too!
  */
  def writeHtmlToFile(path: String): Unit = {
    writeToFile(path, generateHtml().toString)
  }

  /**
   * Read in `items.json`
  */
  def readIndex(): blog.Result[HtmlText] = {
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
    
    val raw = readFile(Path.items)
    val parseResult = decode[List[page.Item]](raw)
    println(parseResult)
    Right(div())
  }

}
